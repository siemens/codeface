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
if (!fileName) {
    console.log('Usage: node ' + process.argv[1] + ' FILENAME' + ' [warn|error|info|trace]');
    process.exit(1);
}

// set log level
var logLevel = process.argv[3];
if (logLevel == 'error') {
    logger.debugLevel = 'error';
} else if (logLevel == 'warn') {
    logger.debugLevel = 'warn';
} else if (logLevel == 'info' || !logLevel) {
    logger.debugLevel = 'info';
} else if (logLevel == 'trace') {
    logger.debugLevel = 'trace';
} else {
    console.log('Usage: node ' + process.argv[1] + ' FILENAME' + ' [warn|error|info]');
    process.exit(1);
}

console.log('Starting REST Service with logging level "' + logger.debugLevel + '"');

// read configuration file & parse the YAML
var fs = require('fs');
var file = fs.readFileSync(fileName, "utf8");
var config = yaml.load(file);

// initilize the express module and the connection to mysql
var app = express();

logger.log('info', 'Connecting to MySQL at ' + config.dbuser + '@' + config.dbhost + ' database ' + config.dbname)
var db_config = {
    host: config.dbhost,
    user: config.dbuser,
    password: config.dbpwd,
    database: config.dbname
};
var pool = mysql.createPool(db_config)

app.configure(function() {
    // used to parse JSON object given in the body request
    app.use(express.bodyParser());
});

/**
 * Obtain a connection from the pool, check if it works, and then execute
 * the given function on it. If an exception occurs, log it and send it in the response,
 * and finally end the connection and return it to the pool
 */
function withCheckedConnection(response, func) {
    pool.getConnection(function(err, connection) {
        if (err) {
            msg = 'MySQL connection error: ' + err;
            logger.log('error', msg);
            response.send(msg);
            connection.end();
        } else {
            try {
                func(connection);
            } catch (exception) {
                msg = 'Exception: ' + exception.message
                logger.log('error', msg);
                response.send(msg);
                connection.destroy();
            }
        }
    });
};

/**
 * HTTP GET /users
 * Returns: the list of users in JSON format
 */
app.get('/getUsers', function(request, response) {
    logger.log('info', "/getUsers");
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM person;', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /user/:id
 * Param: :id is the unique identifier of the user you want to retrieve
 * Returns: the user with the specified :id in a JSON format
 */
app.get('/getUser/:id', function(request, response) {
    logger.log('info', "/getUser/:id");
    var taskId = request.params.id;
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM person WHERE id=' + taskId + ';', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /getReleaseTimelines
 * Param: :projectID (Integer) is the id of the project the timeseries belong to
 * Returns: the list of ReleaseTimelines belonging to the given project in JSON format
 */
app.get('/getReleaseTimelines/:projectID', function(request, response) {
    logger.log('info', '/getReleaseTimelines/:projectID');
    var projectID = request.params.projectID;
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM release_timeline where projectId = ' + projectID + ';', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /getReleaseTimelinesForInterval
 * Param: :projectID (Integer) is the id of the project the timeseries belong to
 * Param: :startTimestamp (timestanp) begin of intervall
 * Param: :endTimestamp (timestanp) end of intervall
 * Returns: the list of ReleaseTimelines belonging to the given project in JSON format
 */
app.get('/getReleaseTimelines/:projectID/:startTimestamp/:endTimestamp', function(request, response) {
    logger.log('info', '/getReleaseTimelines/:projectID/:startTimestamp/:endTimestamp');
    var projectID = request.params.projectID;
    var begin = request.params.startTimestamp;
    var end = request.params.endTimestamp;
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM release_timeline where projectId = ' + projectID + ' and date >= \'' + begin + '\' and date <= \'' + end + '\';', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /getPlotBinData
 * Param: :projectID (Integer) is the id of the project the data belongs to
 * Param: :plotName (String) is the name of the plot
 * Param: :plotType (String) is the type of the plot
 * Returns: the list of ReleaseTimelines belonging to the given project in JSON format
 */
app.get('/getPlotBinData/:projectID/:plotName/:plotType', function(request, response) {
    logger.log('info', '/getPlotBinData/:projectID/:plotName/:plotType');
    var projectID = request.params.projectID;
    var plotName = request.params.plotName;
    var plotType = request.params.plotType;
    withCheckedConnection(response, function(connection) {
        connection.query('select plotId, type, data from plot_bin, plots where plotId = id and projectId = ' + projectID + ' and name = \'' + plotName + '\' and type = \'' + plotType + '\';', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /getProjects
 * Returns: the list of projects in JSON format containing id, name, analysisMethod, analysisTime
 */
app.get('/getProjects', function(request, response) {
    logger.log('info', '/getProjects');
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM project;', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /projectByName/:name
 * Param: :name (String) is the name of the projects to query for
 * Returns: the list of projects with that name in JSON format containing id, name, analysisMethod, analysisTime
 */
app.get('/getProjectsByName/:name', function(request, response) {
    logger.log('info', '/getProjectsByName/:name');
    var name = request.params.name;
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM project where name = \'' + name + '\';', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /projectsByNameAndMethod/:name/:method
 * Param: :name (String) is the name of the projects to query for
 * Param: :method (String) is the name of the analysis method used for the project analysis
 * Returns: the list of projects with that name in JSON format containing id, name, analysisMethod, analysisTime
 */
app.get('/getProjectsByNameAndMethod/:name/:method', function(request, response) {
    logger.log('info', '/getProjectsByNameAndMethod/:name/:method');
    var name = request.params.name;
    var method = request.params.method;
    withCheckedConnection(response, function(connection) {
        connection.query('SELECT * FROM project where name = \'' + name + '\' and analysisMethod = \'' + method + '\';', function(error, rows, fields) {
            response.end(JSON.stringify(rows));
        });
        connection.end();
    });
});

/**
 * HTTP GET /getTimeSeriesData/:projectID/:plotName
 * Param: :projectID (Integer) is the id of the project the timeseries belongs to
 * Param: :plotName (String) is the identifiing name of the plot the timeseries belongs to
 * Returns: the list of data for that plot ordered ascending by time in JSON format containing plotid, time, value, value_scaled
 */
app.get('/getTimeSeriesData/:projectID/:plotName', function(request, response) {
    logger.log('info', '/getTimeSeriesData/:projectID/:plotName');
    var projectID = request.params.projectID;
    var plotName = request.params.plotName;
    withCheckedConnection(response, function(connection) {
        connection.query('select plotId, time, value, value_scaled from timeseries, plots where plotId = id and projectId = ' + projectID + ' and name = \'' + plotName + '\' order by time asc;', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * HTTP GET /getTimeSeriesDataForInterval/:projectID/:plotName/:startTimestamp/:endTimestamp
 * Param: :projectID (Integer) is the id of the project the timeseries belongs to
 * Param: :plotName (String) is the identifiing name of the plot the timeseries belongs to
 * Param: :startTimestamp (timestanp) begin of intervall
 * Param: :endTimestamp (timestanp) end of intervall
 * Returns: the list of data existing in the given intervall for that plot ordered ascending by time in JSON format containing plotid, time, value, value_scaled
 */
app.get('/getTimeSeriesDataForInterval/:projectID/:plotName/:startTimestamp/:endTimestamp', function(request, response) {
    logger.log('info', '/getTimeSeriesDataForInterval/:projectID/:plotName/:startTimestamp/:endTimestamp');
    var projectID = request.params.projectID;
    var plotName = request.params.plotName;
    var begin = request.params.startTimestamp;
    var end = request.params.endTimestamp;
    withCheckedConnection(response, function(connection) {
        connection.query('select plotId, time, value, value_scaled from timeseries, plots where plotId = id and projectId = ' + projectID + ' and name = \'' + plotName + '\' and time >= \'' + begin + '\'  and time <= \'' + end + '\' order by time asc;', function(error, rows, fields) {
            response.jsonp(rows);
        });
        connection.end();
    });
});

/**
 * LOCK Person table
 * There is no unlock function, since all locks are dropped anyway
 * once the connection is destroyed.
 */

function lockTablePerson(connection) {
    logger.log('trace', 'lockTablePerson');
    // Lock table Person
    connection.query('LOCK TABLE person WRITE;', function(error, ret, fields) {
        if (error) {
            logger.log('error', 'Lock returned error: ' + error);
        }
    });
    return true;
}

/**
 * Query for ID given a name, email, projectID and open connection
 * If no fatal errors have occurred, the callback function is called
 * with the rows returned from the database; containing the id or ids
 * of the selected entries.
 */

function checkedWithID(name, email, projectID, response, connection, callback) {
    if (name && email) {
        // First, try to get an existing database that matches both name and email
        q = 'SELECT id FROM person WHERE projectId=\'' + projectID +
            '\' AND name=\'' + name +
            '\' AND \'' + email + '\' IN (email1, email2, email3, email4, email5);'
    } else if (email) {
        // First, try to get an existing database that matches email
        // Technically, this can match more than one person if the email and
        // the name have been inserted separately, so specify LIMIT 1
        q = 'SELECT id FROM person WHERE projectId=\'' + projectID +
            '\' AND \'' + email + '\' IN (email1, email2, email3, email4, email5)' +
            ' ORDER BY id LIMIT 1;'
    } else { // name is set
        // First, try to get an existing database that matches the same name
        q = 'SELECT id FROM person WHERE projectId=\'' + projectID +
            '\' AND name=\'' + name + '\' ORDER BY id LIMIT 1;'
    }
    logger.log('trace', q)
    connection.query(q, function(error, rows, fields) {
        if (error) {
            msg = 'MySQL error: ' + error;
            logger.log('error', msg);
            response.send(msg);
            connection.destroy();
        } else {
            try {
                callback(rows);
            } catch (exception) {
                msg = 'Exception: ' + exception.message;
                logger.log('error', msg);
                response.send(msg);
                connection.destroy();
            }
        }
    });
}

/**
 * GET/POST USER ID from DB
 *
 * This function retrieves or - if she is not present - inserts a user
 * into the database. This function provides the 'fastpath'; a quick
 * return if a matching entry is found. If the database needs to be updated,
 * the app.getOrUpdateUserInDB function is called from here
 */
app.getUserFromDB = function(name, email, projectID, response) {
    // make sure to escape some characters in strings like e.g. quote char
    var patt = /'/g;
    if (name) {
        name = name.replace(patt, "\\'");
    }
    if (email) {
        email = email.replace(patt, "\\'");
    }
    withCheckedConnection(response, function(connection) {
        // Return an error if ProjectId or name and email have been empty
        if (!projectID) {
            logger.log('error', 'input error: projectID missing');
            response.end(JSON.stringify("missing ProjectID"));
            connection.end()
        } else if ((!name) && (!email)) {
            logger.log('error', 'input error: name and email missing');
            response.end(JSON.stringify("missing name and email"));
            connection.end()
        } else {
            checkedWithID(name, email, projectID, response, connection, function(rows) {
                if (rows.length == 1) {
                    // Regular response
                    var id = { id : rows[0].id };
                    response.end(JSON.stringify(id));
                } else if (rows.length == 0) {
                    if (!email) {
                        logger.log('error', 'name not found in database!');
                        response.end(JSON.stringify("name not found"));
                    } else {
                        app.getOrUpdateUserInDB(name, email, projectID, response)
                    }
                } else {
                    logger.log('error', 'database error: duplicate entries!');
                    response.end(JSON.stringify("duplicate entries"));
                }
                connection.end()
            });
        }
    });
}

/**
 * Get, add or update a user in the DB given the name and email
 * This function uses MySQL table locks to synchronise.
 */
app.getOrUpdateUserInDB = function(name, email, projectID, response) {
    // Assertion: email must be set at this point
    logger.log('info', 'inserting/updating user ' + name + ' <' + email + '>');
    withCheckedConnection(response, function(connection) {
        try {
            lockTablePerson(connection)
            // From this point until connection.destroy(), all queued MySQL
            // requests on this connection operate under the table lock
            // Note that we first have to check again if we missed an update
            // before we got the lock.
            checkedWithID(name, email, projectID, response, connection, function(rows) {
                if (rows.length == 1) {
                    // Someone else did the insert for us. Just return and destroy the connection
                    var id = { id : rows[0].id };
                    response.end(JSON.stringify(id));
                    connection.destroy()
                } else if (rows.length == 0) {
                    // Now we have to insert or update the database

                    // This function will terminate the connection, and log any MySQL error
                    fail_on_error = function(error, info) {
                        if (error) {
                            logger.log('trace', info)
                            msg = 'MySQL Error: ' + error
                            logger.log('error', msg)
                            response.end(JSON.stringify(msg));
                        }
                        connection.destroy()
                    }

                    if (name && email) {
                        // First, try to find a set by name
                        checkedWithID(name, null, projectID, response, connection, function(rows) {
                            if (rows.length >= 1) {
                                // Add this email to the end of the email list if it is not yet in the list
                                q = 'UPDATE person SET email5=CASE WHEN email5 IS NULL AND email4 IS NOT NULL THEN \'' + email + '\' ELSE email5 END, ' +
                                    '  email4=CASE WHEN email4 IS NULL AND email3 IS NOT NULL THEN \'' + email + '\' ELSE email4 END, ' +
                                    '  email3=CASE WHEN email3 IS NULL AND email2 IS NOT NULL THEN \'' + email + '\' ELSE email3 END, ' +
                                    '  email2=CASE WHEN email2 IS NULL THEN \'' + email + '\' ELSE email2 END  ' +
                                    ' WHERE id=\'' + rows[0].id + '\' LIMIT 1;'
                                logger.log('trace', q)
                                connection.query(q, fail_on_error)
                                var id = { id : rows[0].id };
                                response.end(JSON.stringify(id));
                            } else {
                                // Now find a set by email
                                checkedWithID(null, email, projectID, response, connection, function(rows) {
                                    if (rows.length >= 1) {
                                        // Update the name of this entry
                                        q = 'UPDATE person SET name=\'' + name + '\' WHERE id=\'' + rows[0].id + '\' LIMIT 1;'
                                        logger.log('trace', q)
                                        connection.query(q, fail_on_error)
                                        var id = { id : rows[0].id };
                                        response.end(JSON.stringify(id));
                                    } else {
                                        // Now we have to insert a new user
                                        q = 'INSERT INTO person (projectId, name, email1) VALUES (\'' + projectID + '\',\'' + name + '\', \'' + email + '\');'
                                        logger.log('trace', q)
                                        connection.query(q, function(error, info) {
                                            connection.destroy();
                                            if (error) {
                                                msg = 'MySQL error: ' + error;
                                                logger.log('error', msg);
                                                response.send(msg);
                                            } else {
                                                var id = { id : info.insertId };
                                                response.end(JSON.stringify(id));
                                            }
                                        });
                                    }
                                });
                            }
                        });
                    } else { // only email is set
                        q = 'INSERT INTO person (projectId, email1) VALUES (\'' + projectID + '\',\'' + email + '\');'
                        connection.query(q, function(error, info) {
                            connection.destroy();
                            if (error) {
                                msg = 'MySQL error: ' + error;
                                logger.log('error', msg);
                                response.send(msg);
                            } else {
                                var id = { id : info.insertId };
                                response.end(JSON.stringify(id));
                            }
                        });
                    }
                } else {
                    logger.log('error', 'database error: duplicate entries!');
                    response.end(JSON.stringify("duplicate entries"));
                    connection.destroy()
                }
            });
        } catch (exception) {
            msg = 'Exception: ' + exception.message
            logger.log('error', msg)
            response.send(msg);
        }
    });
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
    logger.log('trace', 'POST User ID request: ' + request.body);
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
logger.log('info', 'Starting to listen on ' + config.nodejsHostname + ':' + config.nodejsPort)
app.listen(config.nodejsPort, config.nodejsHostname); //to port & hostname on which the express server listen
