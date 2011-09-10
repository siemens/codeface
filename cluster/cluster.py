#! /usr/bin/env python
##! /usr/local/bin/pypy  ### NOTE: pypy fails indeterministically sometimes
# Prepare base data for the commit cluster analysis
# (statistical operations will be carried out by R)
# WM 5. Aug 2011

from VCS import gitVCS
import shelve
import csv
import os
import os.path
#from commit_analysis import createCumulativeSeries, createSeries
import kerninfo
from progressbar import *
from PersonInfo import PersonInfo
from commit_analysis import *
import pickle
import re           # TODO: This and the next can go once idManager is used
from email.utils import parseaddr

def _abort(msg):
    print(msg + "\n")
    sys.exit(-1)

# TODO: Generating the release candidate ranges should be automated
def createDB(filename, revrange, subsys_descr, rcranges=None):
    git = gitVCS();
    git.setRepository("/Users/wolfgang/git-repos/linux/.git")
    git.setRevisionRange(revrange[0], revrange[1])
    git.setSubsysDescription(subsys_descr)
    if rcranges != None:
        git.setRCRanges(rcranges)
    git.extractCommitData()

    print("Shelfing the VCS object")
    output = open(filename, 'wb')
    pickle.dump(git, output, -1)
    output.close()
    print("Finished with pickle")

#    d = shelve.open(filename)
#    d["git"] = git
#    d.close()

def readDB(filename):
#    k = shelve.open(filename)
#    git = k["git"]
#    k.close()
    pkl_file = open(filename, 'rb')
    git = pickle.load(pkl_file)
    pkl_file.close()

    return(git)

# TODO: Fold this mechanism into a class
ID = 0
fixup_emailPattern = re.compile(r'([^<]+)\s+<([^>]+)>')
commaNamePattern = re.compile(r'([^,\s]+),\s+(.+)')
def getPersonID(addr, person_ids, persons, subsys_names):
    """Create a unique ID from a contributor

    Multiple identities are detected using heuristics. We do not use
    an explicit database because the method is supposed to be applicable for
    a wide range of projects.
    """
    # TODO: Locking should be done properly, read up the python docs
    # to see what's available. Essentially, only one thread at a
    # time should be able to assign IDs

    global ID
#    lock = threading.Lock()

    (name, email) = parseaddr(addr)
    # The eMail parser cannot handle Surname, Name <email@domain.tld> properly.
    # Provide a fixup hack for this case
    if (name == "" and email.count("@") == 0):
        m = re.search(fixup_emailPattern, addr)
        if m:
            name = m.group(1)
            email = m.group(2)
            m2 = re.search(commaNamePattern, name)
            if m2:
                name = "{0} {1}".format(m2.group(2), m2.group(1))

        else:
            # In this case, no eMail address was specified.
            name = addr
            email = "could.not@be.resolved.tld"
    
    email = email.lower()

    if name != "":
        name_known = person_ids.has_key(name)
    else:
        name_known = False

    if email != "":
        email_known = person_ids.has_key(email)
    else:
        print("WARNING: Developer {0} has no email address?!".format(name))
        email_known = False

    if not(name_known) and not(email_known):
        person_ids[name] = ID
        person_ids[email] = ID
        persons[ID] = PersonInfo(subsys_names, ID, "{0} <{1}>".format(name, email))
        ID += 1
    elif name_known and not(email_known):
        # Person with multiple eMail addresses (we assume that the name of
        # each developer is unique. Resolving the general case would require extra
        # information)
        person_ids[email] = person_ids[name]
    elif  email_known and not(name_known):
        # Different orthographic variants if the name
        person_ids[name] = person_ids[email]

    return person_ids[name]

def computeSubsysAuthorSimilarity(cmt_subsys, author):
    """ Compute a similarity measure between commit and commit author
    in terms of touched subsystems (ranges from 0 to 1)."""
    
    # NOTE: This is naturally subjective because there are many
    # other choices for defining a similarity measure.
    asf = author.getSubsysFraction()

    sim = 0
    for (subsys_name, subsys_touched) in cmt_subsys.iteritems():
        sim = max(sim, asf[subsys_name]*subsys_touched)

    if  sim > 1:
        _abort("Internal error: Author/Subsystem similarity exceeds one.")

    if sim == 0:
        print("Zero similarity for {0}".format(author.getName()))
        for (subsys_name, subsys_touched) in cmt_subsys.iteritems():
            print("    {0}: {1}, {2}".format(subsys_name, subsys_touched, asf[subsys_name]))

    return sim

def computeAuthorAuthorSimilarity(auth1, auth2):
    """Compute a similarity measure (between 0 and 1) of two authors.

    The measure is derived from the subsystem activitiy of the authors.
    """
    # NOTE: Again, the definition of the similarity measure is subjectiv

    frac1 = auth1.getSubsysFraction()
    frac2 = auth2.getSubsysFraction()
    count = 0
    sim = 0

    for (subsys_name, fraction) in frac1.iteritems():
        if fraction != 0 and frac2[subsys_name] != 0:
            count += 1
            sim += fraction + frac2[subsys_name] # UB: 2

    if count > 0:
        sim /= float(2*count)
        
    if  sim > 1:
        _abort("Internal error: Author/Author similarity exceeds one.")
    
    return sim

def createStatisticalData(cmtlist, person_ids, persons, subsys_names):
    """Generate a person connection data structure from a list of commits

    cmtlist is the list of commits.
    person_ids is a (passed in) hash table that is sucessively filled with
            mappings from an author/email address
    persons is also passed in, and is filled with mappings from IDs to
            PersonInfo instances
    subsys_names is a list with the names of all subsystems.
    """
    
    # To obtain the full collaboration information, we need to have
    # the complete list of commits. This is why we don't compute the
    # information during the first parsing stage, but parse the
    # information in a second pass

    # With every person, we can associate statistical information into
    # which subsystems he/she typically commits, with whom he collaborates,
    # and so on. From this, we can infer further information for each
    # commit, for instance how many people working on different subststems
    # have signed off the commit, of how important the people who sign off
    # the commit are.

    # NOTE: These operations would be really apt for parallelisation
    # once every person has a unique ID (or if a thread safe ID assignment 
    # mechanism is used). However, the first pass takes much much
    # longer, so optimisations are better spent there.
    widgets = ['Pass 2/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
    pbar = ProgressBar(widgets=widgets, maxval=len(cmtlist)).start()
    
    for i in range(0, len(cmtlist)):
        cmt = cmtlist[i]

        if i % 10 == 0:
            pbar.update(i)
            
        # Second, infer in which way people are involved in the commit
        ID = getPersonID(cmt.getAuthorName(), person_ids, persons, subsys_names)
        pi = persons[ID]
        cmt.setAuthorPI(pi)

        # Also note on a per-author basis which subsystems were touched
        pi.addCommit(cmt)

        # Remember which subsystems the person touched in the role as an author
        pi.addPerformTagRelation("author", ID, cmt)
        tag_pi_list = {}
        
        for tag in tag_types:
            tag_pi_list[tag] = []
            for name in getInvolvedPersons(cmt, [tag]):
                relID = getPersonID(name, person_ids, persons, subsys_names)
                tag_pi_list[tag].append(persons[relID])
                
                # Authors typically sign-off their patches,
                # so don't count this as a relation.
                if (relID != ID):
                    # Author received a sign-off etc. by relID
                    pi.addReceiveTagRelation(tag, relID)

                    # relID did a sign-off etc. to author
                    persons[relID].addPerformTagRelation(tag, ID, cmt)

        cmt.setTagPIs(tag_pi_list)

    # Now that all information on tags is available, compute the normalised
    # statistics. While at it, also compute the per-author commit summaries.
    for (key, person) in persons.iteritems():
        person.computeTagStats()
        person.computeCommitStats()

    # Do another iteration pass over the commits, and compute data
    # that require the first pass results as input.
    # TODO: Think about what exactly we want to achieve here, because
    # this is the basis for data analysis with R.
    for cmt in cmtlist:
        # Compute similarity between the subsystems touched by the
        # commit, and the subsystems the author typically deals with
        author_pi = cmt.getAuthorPI()
        sim = computeSubsysAuthorSimilarity(cmt.getSubsystemsTouched(),
                                            author_pi)
        cmt.setAuthorSubsysSimilarity(sim)
        
        # Compute the similarity between author and taggers, and between
        # commit and taggers
        count = 0
        atsim = 0 # Author-tagger similarity
        tssim = 0 # Tagger-subsys similarity
        
        for (key, pi_list) in cmt.getTagPIs().iteritems():
            for pi in pi_list:
                count += 1
                atsim += computeAuthorAuthorSimilarity(author_pi, pi)
                tssim += \
                    computeSubsysAuthorSimilarity(cmt.getSubsystemsTouched(),
                                                  pi)

        if count > 0:
            atsim /= float(count)
            tssim /= float(count)

        cmt.setAuthorTaggersSimilarity(atsim)
        cmt.setTaggersSubsysSimilarity(tssim)

    return None

def emitStatisticalData(cmtlist, person_ids, persons, subsys_names, outdir):
    """Save the available information for further statistical processing.

    Several files are created in outdir:
    - Information about the commits proper (commits.txt)
    - Names/ID associations (ids.txt). This file also contains
      the per-author total of added/deleted/modified lines
    - Connection between the developers derived from commit tags (tags.txt)"""

    # Save information about the commits
    # NOTE: We could care about different diff types, but currently,
    # we don't. There are strong indications that it does not matter
    # at all anyway which diff algorithm we use
    out = open(os.path.join(outdir, "commits.txt"), 'wb')

    # Construct the header...
    header = "ID	ChangedFiles	AddedLines	DeletedLines	DiffSize	CmtMsgLines	CmtMsgBytes	NumSignedOffs	NumTags	"
    header += "\t".join(subsys_names + ["general"])
    header += "\tTotalSubsys"
    header += "\tSubsys"
    header += "\tinRC"
    header += "\tAuthorSubsysSimilarity"
    header += "\tAuthorTaggersSimilarity"
    header += "\tTaggersSubsysSimilarity"
    print >>out, header

    # ... and write the values proper
    fmtstr = "\t".join(["{" + str(x) + "}" for x in range(0,9)])
    for cmt in cmtlist:
        outstr = fmtstr.format(cmt.id,
                               cmt.getChangedFiles(0),
                               cmt.getAddedLines(0),
                               cmt.getDeletedLines(0),
                               cmt.getAddedLines(0) + cmt.getDeletedLines(0),
                               cmt.getCommitMessageLines(),
                               cmt.getCommitMessageSize(),
                               getSignoffCount(cmt),
                               getSignoffEtcCount(cmt))

        subsys_touched = cmt.getSubsystemsTouched()
        subsys_count = 0
        for subsys in subsys_names + ["general"]:
            outstr += "\t{0}".format(subsys_touched[subsys])
            subsys_count += subsys_touched[subsys]
            if subsys_touched[subsys] == 1:
                # If the commit touches more than one subsys, this
                # is obviously not unique.
                subsys_name = subsys

        outstr += "\t{0}".format(subsys_count)
        outstr += "\t{0}".format(subsys_name)
        
        if cmt.getInRC():
            outstr += "\t1"
        else:
            outstr += "\t0"

        outstr += "\t{0}".format(cmt.getAuthorSubsysSimilarity())
        outstr += "\t{0}".format(cmt.getAuthorTaggersSimilarity())
        outstr += "\t{0}".format(cmt.getTaggersSubsysSimilarity())

        out.write(outstr + "\n")
        # TODO: Continue writing here. Include at least
        # signoff-info (subsys info of signers)
        # similarity_between_author_and_signers
        # predominantly add, remove, or modify code (3-level factor)
    out.close()

    ##############
    # Save id/name associations together with the per-author summary statistics
    id_writer = csv.writer(open(os.path.join(outdir, "ids.txt"), 'wb'),
                           delimiter='\t',
                           quotechar='\\', quoting=csv.QUOTE_MINIMAL)
    
    # TODO: Split the developer name into name and email address so that
    # R does not need to repeat parsing the info. When no name is available,
    # PersonInfo should just substitute the eMail address for the name
    for id in sorted(persons.keys()):
        pi = persons[id]
        cmt_stat = pi.getCommitStats()
        added = cmt_stat["added"]
        deleted = cmt_stat["deleted"]
        numcommits = cmt_stat["numcommits"]
        id_writer.writerow([id, pi.getName(), added, deleted, added + deleted,
                             numcommits])
    
    ##############
    # Store the adjaceny matrix for developer tagging, i.e., create
    # a NxN matrix in which the entry a_{i,j} denotes how often developer
    # j was tagged by developer i
    # NOTE: This produces a sparse matrix, but since the number
    # of developers is only a few thousand, it will likely not pay
    # off to utilise this fact for more efficient storage.

    out = open(os.path.join(outdir, "tags.txt"), 'wb')
    idlist = sorted(persons.keys())
    # Header
    out.write("# " +
              "\t".join([str(persons[elem].getName()) for elem in idlist]) +
              "\n")
    
    # Matrix. The sum of all elements in row N describes how many
    # tags id N has received. The sum of column N states how many
    # tags were given by id N to other developers.
    for id_receiver in idlist:
        out.write("\t".join(
            [str(persons[id_receiver].getTagsReceivedByID(id_sender))
               for id_sender in idlist]) + "\n")

    out.close()

    return None

    

###########################################################################
# Main part
###########################################################################

def performAnalysis(dbfilename, revrange, subsys_descr, create_db, outdir, rcranges=None):
    if create_db == True:
        print("Creating data base for {0}..{1}").format(revrange[0],
                                                        revrange[1])
        createDB(dbfilename, revrange, subsys_descr, rcranges)
        
        
    print("Reading from data base...")
    git = readDB(dbfilename)
    cmtlist = git.extractCommitData("__main__")

    # Determine in which ways the authors are connected with each other,
    # plus some more statistical information
    person_ids = {} # NOTE: May only be accessed via getPersonID
    persons = {}
    createStatisticalData(cmtlist, person_ids, persons,
                          kerninfo.subsysDescrLinux.keys())

    # Save the results in text files that can be further processed with
    # statistical software, that is, GNU R
    emitStatisticalData(cmtlist, person_ids, persons,
                        kerninfo.subsysDescrLinux.keys(), outdir)
    
##################################################################
def doKernelAnalysis(rev, outbase, create_db):
    from_rev = "v2.6.{0}".format(rev)
    to_rev = "v2.6.{0}".format(rev+1)
    rc_start = "{0}-rc1".format(to_rev)

    outdir = os.path.join(outbase, str(rev))

    if not os.path.exists(outdir):
        try:
            os.mkdir(outdir)
        except os.error as e:
            print("Could not create output dir {0}: {1}".format(outdir, e.strerror))
            exit(-1)

    filename = os.path.join(outbase, "linux-{0}-{1}".format(rev,rev+1))
    performAnalysis(filename, [from_rev, to_rev], kerninfo.subsysDescrLinux,
                    create_db, outdir, [[rc_start, to_rev]])


outbase = "/Users/wolfgang/papers/csd/cluster/res/"
#rev = 34
#doKernelAnalysis(34, outbase, False)
#exit(0)

for rev in range(30,33):
    ID = 0
    doKernelAnalysis(rev, outbase, True)

exit(0)


########################### Some tests/examples ################
# Which roles did a person fulfill?
for person in persons.keys()[1:10]:
    tag_stats = persons[person].getTagStats()
    print("Name: {0}".format(persons[person].getName()))
    stats_str = ""
    for (tag, count) in tag_stats.iteritems():
        stats_str += "({0}, {1}) ".format(tag, count)
    print("   {0}".format(stats_str))

# TODO: Associate the commits done as author with a person. This way,
# we can easily reproduce the lwn statistics.

# Which subsystems was the person involved in as author?
for person in persons.keys()[1:10]:
    subsys_stats = persons[person].getSubsysStats()["author"]
    print("Name: {0}".format(persons[person].getName()))

    stats_str = ""
    for (subsys, count) in subsys_stats.items():
        stats_str += "({0}, {1}) ".format(subsys, count)

    print("   {0}".format(stats_str))
            
# TODO: Compute the grand total for all subsystems except "author"
# to see in which ones the developer was involved as a non-author

# With which others did the person cooperate?
print("ID\tcdate\tAddedLines\tSignoffs\tCmtMsgSize\tChangedFiles\t")
for ID, pi  in persons.items()[1:10]:
    print("ID: {0}, name: {1}".format(pi.getID(), pi.getName()))
    print("  Signed-off-by:")
    for relID, count in pi.getPerformTagRelations("Signed-off-by").iteritems():
        print("    person: {0}, count: {1}".format(persons[relID].getName(),
                                                    count))
    
    

