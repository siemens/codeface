#! /usr/bin/env python2.7
# -*- coding: utf-8 -*-

# Short python example to test the ID service
import httplib
import urllib
import json

server="localhost"
port=8080

def query_user_id(conn, projectID, name, email):
    
    params = urllib.urlencode({'projectID': projectID,
                               'name': name,
                               'email': email})
    headers = {"Content-type": "application/x-www-form-urlencoded; charset=utf-8",
               "Accept": "text/plain"}

    conn.request("POST", "/post_user_id", params, headers)
    res = conn.getresponse()
    print "Response:", res.status, res.reason

    # TODO: We should handle errors by throwing an exception instead
    # of silently ignoring them
    id = json.loads(res.read())["id"]
    return(id)

    
#########################
conn = httplib.HTTPConnection(server, port)

# Insert/Query some test users
data = query_user_id(conn, 1, "Wolfgang Mauerer",
                     "wolfgang.mauerer@siemens.com")
print("Query 1 returned ID: {0}\n".format(data))

# Another email address for the same user
data = query_user_id(conn, 1, "Wolfgang Mauerer",
                     "wm@linux-kernel.net")
print("Query 1.5 returned ID: {0}\n".format(data))

# Test if UTF-8 works
data = query_user_id(conn, 1, "Wölfgang Mäüerer",
                     "wolfgng.maeueerer@siemens.com")
print("Query 2 returned ID: {0}\n".format(data))

# Add a completely different third entry
data = query_user_id(conn, 1, "Sepp Testhuber",
                     "sepp.huber@ge-fanuc.com")
print("Query 3 returned ID: {0}\n".format(data))

## Query example
conn.request("GET", "/users")
res = conn.getresponse()
print "Status: {0} ({1})".format(res.status,
                                 res.reason)
print("GET query returned {0}\n".format(res.read()))

conn.close()
