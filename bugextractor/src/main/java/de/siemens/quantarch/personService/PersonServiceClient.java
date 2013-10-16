/*
 * This file is part of Codeface. Codeface is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, version 2.

 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 * Copyright 2013 by Siemens AG. All Rights Reserved.
 */

package de.siemens.quantarch.personService;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import net.sf.json.JSONObject;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;

public final class PersonServiceClient {

	public static final long getPerson(String name, String email,
			long projectId, String personServiceUrl) {
		long userId = 0;
		HttpClient client = new DefaultHttpClient();
		HttpPost post = new HttpPost(personServiceUrl + "/post_user_id");
		try {
			List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(1);
			nameValuePairs.add(new BasicNameValuePair("projectID", Long
					.toString(projectId)));
			nameValuePairs.add(new BasicNameValuePair("name", name));
			nameValuePairs.add(new BasicNameValuePair("email", email));
			post.setEntity(new UrlEncodedFormEntity(nameValuePairs));

			HttpResponse response = client.execute(post);
			BufferedReader rd = new BufferedReader(new InputStreamReader(
					response.getEntity().getContent()));

			// read output
			StringBuilder builder = new StringBuilder();
			String line = null;
			while ((line = rd.readLine()) != null) {
				builder.append(line);
			}

			JSONObject json = JSONObject.fromObject(builder.toString());
			userId = json.getLong("id");
			return userId;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return userId;
	}
}
