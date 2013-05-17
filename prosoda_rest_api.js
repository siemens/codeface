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

// initilize the express module and the connection to mysql
var app = express();
var connection = mysql.createConnection({ 
    host : 'localhost',
    user: 'quantarch',
    password: 'quantarch',
    database: 'quantarch'
}); 

app.configure(function () {
    // used to parse JSON object given in the body request
    app.use(express.bodyParser());
});

/**
 * HTTP GET /users
 * Returns: the list of users in JSON format
 */
app.get('/users', function (request, response) {
    connection.query('SELECT * FROM person;', function (error, rows, fields) { 
        response.end(JSON.stringify(rows)); 
    });
});

/**
 * HTTP GET /user/:id
 * Param: :id is the unique identifier of the user you want to retrieve
 * Returns: the user with the specified :id in a JSON format
 */
app.get('/user/:id', function (request, response) {
    var taskId = request.params.id;
    try {
	connection.query('SELECT * FROM person WHERE id=' + taskId + ';', function (error, rows, fields) { 
	    response.end(JSON.stringify(rows)); 
	});
    } catch (exeception) {
        response.send(404);
    }
    
});

/**
 * LOCK Person table
 * TODO
 */
lockTablePerson = function() {
	var log = { log : 'locking table person'};
    console.log(log);
    // Lock table Person
    connection.query('LOCK TABLE PERSON WRITE;', function (error, ret, fields) { 
		//console.log(ret);
		//console.log(error);
    });
    return true;
}

/**
 * UNLOCK all tables
 * TODO
 */
unlockTablePerson = function() {
	var log = { log : 'unlocking table person'};
    console.log(log);
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
    console.log("app.getUserFromDB");
    try {
	// name and email have been empty
	if  (!projectID)
	{
	    var err = { err : 'input error: projectID missing'};
	    console.log(err);
	    response.end(JSON.stringify(err));
	}
	else if ((!name) && (!email))
	{
	    var err = { err : 'input error: name and email missing'};
	    console.log(err);
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
			var log = { log : 'found id: ' + rows[0].id};
			console.log(log);
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
										var log = { log : 'id: ' +rows[0].id + ', email5 updated'};
										console.log(log);
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
									var log = { log : 'id: ' +rows[0].id + ', email4 updated'};
									console.log(log);
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
								var log = { log : 'id: ' +rows[0].id + ', email3 updated'};
								console.log(log);
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
							var log = { log : 'id: ' +rows[0].id + ', email2 updated'};
							console.log(log);
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
						var log = { log : 'id: ' +rows[0].id + ', email1 updated'};
						console.log(log);
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
						console.log("INCONSISTENCY WARNING: Multiple results for a single person!")

						for (var count = 0; count < rows.length; count++) {
							console.log("  -> ID: " + rows[count].id);
						}
					}
				    // user found: 
				    // update name if not empty in found user
				    if (name){
						connection.query('UPDATE person SET name = \'' + name + '\' WHERE id = ' + rows[0].id + ';', function (error, ret, fields) { 
							// UNLock table Person
							unlockTablePerson();
						});
				    }
					var log = { log : 'id: ' +rows[0].id + ', name updated'};
					console.log(log);
				    
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
						var log = { log : 'new id: ' + info.insertId};
						console.log(log);
						var id = { id : info.insertId};
					    response.end(JSON.stringify(id)); 
					});	
				    } else
				    {
					// insert with email only
					connection.query('INSERT INTO person (projectId, email1) VALUES(\'' + projectID + '\', \'' + email + '\');', function (error, info) { 
					    // UNLock table Person
					    unlockTablePerson();
					    var log = { log : 'new id: ' +info.insertId};
						console.log(log);
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
    } catch (exeception) {
		// try to UNLock table Person
		unlockTablePerson();
		console.log(exeception.message);
        response.send(exeception.message);
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
    console.log(request.body);
    var name = request.body.name;
    var email = request.body.email;
    var projectID = request.body.projectID;
    
    app.getUserFromDB(name, email, projectID, response);
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
 * Start listening on port 8080
 */
app.listen(8080); //to port on which the express server listen
