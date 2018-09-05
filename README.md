# url hosting using servant

# GET /
Returns a list of all links created (in json format)  
A link is of the form `{link: "<unshortened url here>", id: <link id>}`

# GET /get/{id}
The actual redirect. Sends you to the link with the corresponding id.  
400 Error if the id can't be converted to a 64-bit int  
404 Error if there is no link mapped to that id

# POST /url/new
Request body is in the form `"<unshortened url here>"  
Requires the header `"Content-Type: application/json"`  
Returns the new link's id in the response body  
409 Error if the link is already mapped to an id
