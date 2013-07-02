// This file is part of prosoda.  prosoda is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, version 2.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Copyright 2013 by Siemens AG, Stefan Hagen Weber <stefan_hagen.weber@siemens.com>
// All Rights Reserved.

/**
 * Required Modules
 */
var express = require('express');
var mysql = require('mysql');
var yaml = require("js-yaml");
var logger = require('./logger');
var addressparser = require("addressparser");

// get property file name
var fileName = process.argv[2];
if(!fileName)
{
	console.log('Usage: node ' + process.argv[1] + ' FILENAME' + ' [warn|error|info]');
	process.exit(1);
} 

// set log level
var logLevel = process.argv[3];
if(logLevel == 'warn')
{
	logger.debugLevel = 'warn';
} else if(logLevel == 'error' || !logLevel)
{
	logger.debugLevel = 'error';
} else if(logLevel == 'info')
{
	logger.debugLevel = 'info';
} else {
	console.log('Usage: node ' + process.argv[1] + ' FILENAME' + ' [warn|error|info]');
	process.exit(1);
}
console.log('LOG LEVEL: ' + logger.debugLevel);

// read configuration file & parse the YAML
var fs = require('fs');
var file = fs.readFileSync(fileName, "utf8");
var config = yaml.load(file);

// initilize the express module and the connection to mysql
var app = express();
var connection = mysql.createConnection({ 
    host : config.dbhost,
    user: config.dbuser,
    password: config.dbpwd,
    database: config.dbname
}); 

app.configure(function () {
    // used to parse JSON object given in the body request
    app.use(express.bodyParser());
});

/**
 * HTTP GET /users
 * Returns: the list of users in JSON format
 */
app.get('/getUsers', function (request, response) {
	logger.log('info', "/getUsers");
	try {
		connection.query('SELECT * FROM person;', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	 } catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /user/:id
 * Param: :id is the unique identifier of the user you want to retrieve
 * Returns: the user with the specified :id in a JSON format
 */
app.get('/getUser/:id', function (request, response) {
	logger.log('info', "/getUser/:id");
    var taskId = request.params.id;
    try {
	connection.query('SELECT * FROM person WHERE id=' + taskId + ';', function (error, rows, fields) { 
		response.jsonp(rows);
	});
    } catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
    
});

/**
 * HTTP GET /getReleaseTimelines
 * Param: :projectID (Integer) is the id of the project the timeseries belong to
 * Returns: the list of ReleaseTimelines belonging to the given project in JSON format
 */
app.get('/getReleaseTimelines/:projectID', function (request, response) {
	logger.log('info', '/getReleaseTimelines/:projectID');
	var projectID = request.params.projectID;
	try {
		connection.query('SELECT * FROM release_timeline where projectId = ' + projectID + ';', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /getReleaseTimelinesForInterval
 * Param: :projectID (Integer) is the id of the project the timeseries belong to
 * Param: :startTimestamp (timestanp) begin of intervall
 * Param: :endTimestamp (timestanp) end of intervall
 * Returns: the list of ReleaseTimelines belonging to the given project in JSON format
 */
app.get('/getReleaseTimelines/:projectID/:startTimestamp/:endTimestamp', function (request, response) {
	logger.log('info', '/getReleaseTimelines/:projectID/:startTimestamp/:endTimestamp');
	var projectID = request.params.projectID;
	var begin = request.params.startTimestamp;
	var end = request.params.endTimestamp;
	try {
		connection.query('SELECT * FROM release_timeline where projectId = ' + projectID + ' and date >= \'' + begin + '\' and date <= \'' + end + '\';', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /getPlotBinData
 * Param: :projectID (Integer) is the id of the project the data belongs to
 * Param: :plotName (String) is the name of the plot
 * Param: :plotType (String) is the type of the plot
 * Returns: the list of ReleaseTimelines belonging to the given project in JSON format
 */
app.get('/getPlotBinData/:projectID/:plotName/:plotType', function (request, response) {
	logger.log('info', '/getPlotBinData/:projectID/:plotName/:plotType');
	var projectID = request.params.projectID;
	var plotName = request.params.plotName;
	var plotType = request.params.plotType;
	try {
		connection.query('select plotId, type, data from plot_bin, plots where plotId = id and projectId = ' + projectID + ' and name = \'' + plotName	+ '\' and type = \'' + plotType	+ '\';', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /getProjects
 * Returns: the list of projects in JSON format containing id, name, analysisMethod, analysisTime
 */
app.get('/getProjects', function (request, response) {
	logger.log('info', '/getProjects');
	try {
		connection.query('SELECT * FROM project;', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /projectByName/:name
 * Param: :name (String) is the name of the projects to query for
 * Returns: the list of projects with that name in JSON format containing id, name, analysisMethod, analysisTime
 */
app.get('/getProjectsByName/:name', function (request, response) {
	logger.log('info', '/getProjectsByName/:name');
	var name = request.params.name;
	try {
		connection.query('SELECT * FROM project where name = \'' + name + '\';', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /projectsByNameAndMethod/:name/:method
 * Param: :name (String) is the name of the projects to query for
 * Param: :method (String) is the name of the analysis method used for the project analysis
 * Returns: the list of projects with that name in JSON format containing id, name, analysisMethod, analysisTime
 */
app.get('/getProjectsByNameAndMethod/:name/:method', function (request, response) {
	logger.log('info', '/getProjectsByNameAndMethod/:name/:method');
	var name = request.params.name;
	var method = request.params.method;
	try {
		connection.query('SELECT * FROM project where name = \'' + name + '\' and analysisMethod = \'' + method + '\';', function (error, rows, fields) { 
			response.end(JSON.stringify(rows)); 
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /getTimeSeriesData/:projectID/:plotName
 * Param: :projectID (Integer) is the id of the project the timeseries belongs to
 * Param: :plotName (String) is the identifiing name of the plot the timeseries belongs to
 * Returns: the list of data for that plot ordered ascending by time in JSON format containing plotid, time, value, value_scaled
 */
app.get('/getTimeSeriesData/:projectID/:plotName', function (request, response) {
	logger.log('info', '/getTimeSeriesData/:projectID/:plotName');
	var projectID = request.params.projectID;
	var plotName = request.params.plotName;
	try {
		connection.query('select plotId, time, value, value_scaled from timeseries, plots where plotId = id and projectId = ' + projectID + ' and name = \'' + plotName	+ '\' order by time asc;', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * HTTP GET /getTimeSeriesDataForInterval/:projectID/:plotName/:startTimestamp/:endTimestamp
 * Param: :projectID (Integer) is the id of the project the timeseries belongs to
 * Param: :plotName (String) is the identifiing name of the plot the timeseries belongs to
 * Param: :startTimestamp (timestanp) begin of intervall
 * Param: :endTimestamp (timestanp) end of intervall
 * Returns: the list of data existing in the given intervall for that plot ordered ascending by time in JSON format containing plotid, time, value, value_scaled
 */
app.get('/getTimeSeriesDataForInterval/:projectID/:plotName/:startTimestamp/:endTimestamp', function (request, response) {
	logger.log('info', '/getTimeSeriesDataForInterval/:projectID/:plotName/:startTimestamp/:endTimestamp');
	var projectID = request.params.projectID;
	var plotName = request.params.plotName;
	var begin = request.params.startTimestamp;
	var end = request.params.endTimestamp;
	try {
		connection.query('select plotId, time, value, value_scaled from timeseries, plots where plotId = id and projectId = ' + projectID + ' and name = \'' + plotName	+ '\' and time >= \'' + begin + '\'  and time <= \'' + end + '\' order by time asc;', function (error, rows, fields) { 
			response.jsonp(rows);
		});
	} catch (exception) {
        logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
});

/**
 * LOCK Person table
 */
lockTablePerson = function() {
	logger.log('info', 'lockTablePerson');
    // Lock table Person
    connection.query('LOCK TABLE PERSON WRITE;', function (error, ret, fields) { 
		//console.log(ret);
		//console.log(error);
    });
    return true;
}

/**
 * UNLOCK all tables
 */
unlockTablePerson = function() {
	logger.log('info', 'unlockTablePerson');
	// Unlock table person
	connection.query('UNLOCK TABLES;', function (error, ret, fields) { 
		//console.log(ret);
		//console.log(error);
    });
    return true;
}

/**
 * GET/POST USER ID from DB
 * Synchronization is solved via LOCK/UNLOCK of DB table, Note: this has to be tested!!!!
 */
app.getUserFromDB = function(name, email, projectID, response) {
    logger.log('info', 'getUserFromDB');
    try {
	// name and email have been empty
	if  (!projectID)
	{
	    logger.log('error', {error: 'input error: projectID missing'});
	    response.end(JSON.stringify(err));
	}
	else if ((!name) && (!email))
	{
	    logger.log('error', {error: 'input error: name and email missing'});
	    response.end(JSON.stringify(err));
	}
	else
	{
		// make sure to escape some characters in strings like e.g. quote char
		var patt=/'/g;
		if(name){
			name = name.replace(patt,"\\'");
		}
		if (email){
			email = email.replace(patt,"\\'");
		}
		//console.log(name);
		//console.log(email);
		
	    // lock Person table
	    lockTablePerson();
	    // Name AND Mail found --> return existing ID
	    var query = 'SELECT id FROM person WHERE projectId = ' + projectID + ' AND name = \'' + name + '\' AND ( email1 = \'' + email + '\' OR email2 =  \'' + email + '\' OR email3 = \'' + email + '\' OR email4 = \'' + email + '\' OR email5 = \'' + email + '\');';
	    connection.query(query, function (error, rows, fields) { 
		//var log = { log : '1. check'};
		//console.log(log);
		//console.log(rows);
		//console.log(error);
		if (rows.length == 1){
		    // UNLock table Person
		    unlockTablePerson();
			logger.log('info', 'found id: ' + rows[0].id);
		    // user found: return id
		    response.end(JSON.stringify(rows[0]));			
		} 
		else
		{
		    // Name found AND Mail NOT found --> return existing ID, add email
		    connection.query('SELECT id FROM person WHERE projectId = ' + projectID + ' AND name = \'' + name + '\';', function (error, rows, fields) { 
			//var log = { log : '2. check'};
			//console.log(log);
			//console.log(rows);
			//console.log(error);
			if (rows.length == 1){
			    // user found: 
			    // update mail if not empty in found user
			    if(email){
				// check if email is empty
				connection.query('SELECT email1 FROM person WHERE id = ' + rows[0].id + ';', function (error, e1, fields) { 
				    if ((e1[0].email1) && !(e1[0].email1 == "undefined"))
				    {
					// email occupied,
					// check if email2 is empty
					connection.query('SELECT email2 FROM person WHERE id = ' + rows[0].id + ';', function (error, e2, fields) { 
					    if (e2[0].email2)
					    {
						// email2 occupied,
						// check if email3 is empty
						connection.query('SELECT email3 FROM person WHERE id = ' + rows[0].id + ';', function (error, e3, fields) { 
						    if (e3[0].email3)
						    {
							// email3 occupied
							// check if email4 is empty
							connection.query('SELECT email4 FROM person WHERE id = ' + rows[0].id + ';', function (error, e4, fields) { 
							    if (e4[0].email4)
							    {
								// email4 occupied, try email5
								// check if email5 is empty
								connection.query('SELECT email5 FROM person WHERE id = ' + rows[0].id + ';', function (error, e5, fields) { 
								    if (e5[0].email5)
								    {
										// email5 occupied, discard new email
										unlockTablePerson();
										logger.log('info', 'id: ' +rows[0].id + ', email slots full, email discarded');
								    }
								    else
								    {
										// email5 empty, insert
										connection.query('UPDATE person SET email5 = \'' + email + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
											// UNLock table Person
											unlockTablePerson();
											//console.log(ret);
											//console.log(error);
										});
										logger.log('info', 'id: ' +rows[0].id + ', email5 updated');
								    }
								});
							    }
							    else
							    {
									// email4 empty, insert
									connection.query('UPDATE person SET email4 = \'' + email + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
										// UNLock table Person
										unlockTablePerson();
										//console.log(ret);
										//console.log(error);
									});
									logger.log('info', 'id: ' +rows[0].id + ', email4 updated');
							    }
							});
						    }
						    else
						    {
								// email3 empty, insert
								connection.query('UPDATE person SET email3 = \'' + email + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
									// UNLock table Person
									unlockTablePerson();
									//console.log(ret);
									//console.log(error);
								});
								logger.log('info', 'id: ' +rows[0].id + ', email3 updated');
						    }
						});
					    }
					    else
					    {
							// email2 empty, insert
							connection.query('UPDATE person SET email2 = \'' + email + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
								// UNLock table Person
								unlockTablePerson();
								//console.log(ret);
								//console.log(error);
							});
							logger.log('info', 'id: ' +rows[0].id + ', email2 updated');
					    }
					});
				    }
				    else
				    {
						// email empty, insert
						connection.query('UPDATE person SET email1 = \'' + email + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
							// UNLock table Person
							unlockTablePerson();
							//console.log(ret);
							//console.log(error);
						});
						logger.log('info', 'id: ' +rows[0].id + ', email1 updated');
				    }
				});
			    }
			    //console.log(rows);
			    //return id
			    response.end(JSON.stringify(rows[0])); 
			}
			else
			{
			    // Name Not found AND mail found
			    connection.query('SELECT id FROM person WHERE projectId = ' + projectID + ' AND ( email1 = \'' + email + '\' OR email2 =  \'' + email + '\' OR email3 = \'' + email + '\' OR email4 = \'' + email + '\' OR email5 = \'' + email + '\');', function (error, rows, fields) { 
				//var log = { log : '3. check'};
				//console.log(log);
				//console.log(error);
				//console.log("row.length: " + rows.length)
				if (rows.length >= 1){
				    if (rows.length > 1) {
						logger.log('warn', 'INCONSISTENCY WARNING: Multiple results for a single person!')

						for (var count = 0; count < rows.length; count++) {
							logger.log('warn', '  -> ID: ' + rows[count].id);
						}
					}
				    // user found: 
				    // update name if not empty in found user
				    if (name){
						connection.query('UPDATE person SET name = \'' + name + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
							// UNLock table Person
							unlockTablePerson();
							//console.log(ret);
							//console.log(error);
						});
				    }
					logger.log('info', 'id: ' +rows[0].id + ', name updated');
				    
				    //return id
				    //console.log(rows);
				    response.end(JSON.stringify(rows[0])); 
				}
				else
				{
				    // name not found, email not found
				    // insert new user into DB
				    //var log = { log : 'INSERT'};
				    //console.log(log);
				    if(name){
					// insert with name and email
					connection.query('INSERT INTO person (projectId, name, email1) VALUES(\'' + projectID + '\',\'' + name + '\', \'' + email + '\');', function (error, info) { 
						// UNLock table Person
					    unlockTablePerson();
						logger.log('info', 'new id: ' + info.insertId);
						var id = { id : info.insertId};
					    response.end(JSON.stringify(id)); 
					});	
				    } else
				    {
					// insert with email only
					connection.query('INSERT INTO person (projectId, email1) VALUES(\'' + projectID + '\', \'' + email + '\');', function (error, info) { 
					    // UNLock table Person
					    unlockTablePerson();
					    logger.log('info', 'new id: ' + info.insertId);
						var id = { id : info.insertId};
					    response.end(JSON.stringify(id)); 
					});	
				    }
				}
			    });
			}
		    });		
		}
	    });
	}	
    } catch (exception) {
		// try to UNLock table Person
		unlockTablePerson();
		logger.log('error', {error: exception.message});
        response.send(exception.message);
    }
}


/**
 * GET USER ID
 */
app.getUserID = function(request, response) {
    var name = request.params.name;
    var email = request.params.email;
    var projectID = request.params.projectID;
    
    app.getUserFromDB(name, email, projectID, response);
}

/**
 * POST USER ID
 */
app.postUserID = function(request, response) {
    logger.log('info', request.body);
    var name = request.body.name;
    var email = request.body.email;
    var projectID = request.body.projectID;
    
    app.getUserFromDB(name, email, projectID, response);
}

/**
 * POST DECOMPOSE USER ID
 * In contrast to postUserID, the name/email combination can be passed
 * in one string and need not come decomposed into name and email parts.
 */
app.postDecomposeUserID = function(request, response) {
    logger.log('info', request.body);
    var namestr = request.body.namestr;
    var projectID = request.body.projectID;
    var parsed = addressparser(namestr)

    app.getUserFromDB(parsed[0].name, parsed[0].address, projectID, response);
}

/**
 * HTTP GET /get_user_id/:projectID/:name/:email
 * to test it in the browser
 * Param: :projectID ist the id of the project the user belongs to
 * Param: :name is the name of the user
 * Param: :email is the email of the user
 * Returns: the unique identifier (id) of the user with the given name and email in JSON format. If the user did not exist by now it is created (insertId). If no id is found and no new id can be generated an error is returned
 * return example: {"id":15679}
 */
app.get('/get_user_id/:projectID/:name/:email', app.getUserID);

/**
 * HTTP POST /post_user_id/
 * Param: :projectID ist the id of the project the user belongs to
 * Param: :name is the name of the user
 * Param: :email is the email of the user
 * JSON to POST, example with curl: 
 * curl -v -X POST -d "projectID=1&name=Zhang Rui&email=rui.zhang@intel.com" http://localhost:8080/post_user_id
 * Returns: the unique identifier (id) of the user with the given name and email in JSON format. If the user did not exist by now it is created (insertId). If no id is found and no new id can be generated an error is returned
 * return example: {"id":15679}
 */
app.post('/post_user_id', app.postUserID);

/**
 * HTTP POST /post_decompose_user_id/
 * Param: :projectID ist the id of the project the user belongs to
 * Param: :namestr is the combined name/email string
 * JSON to POST, example with curl:
 * curl -v -X POST -d "projectID=1&namestr="Zhang Rui <rui.zhang@intel.com>" http://localhost:8080/post_user_id
 * Returns: the unique identifier (id) of the user with the given name and
 * email in JSON format. If the user did not exist by now it is created
 * (insertId). If no id is found and no new id can be generated an error is
 * returned return example: {"id":15679}
 */
app.post('/post_decompose_user_id', app.postDecomposeUserID);

/**
 * Start listening on port 8080
 */
app.listen(config.nodejsPort, config.nodejsHostname); //to port & hostname on which the express server listen
