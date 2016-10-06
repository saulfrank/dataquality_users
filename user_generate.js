//**** create test user data ****
//**** Saul Frank v1.0 *****

//dependencies faker, json to csv, write to file
var faker = require('faker/locale/en_GB');
var json2csv = require('json2csv');
var fs = require('fs');
faker.locale = "en_GB";


console.log('Generate users');

var i =0;
var user ={};
var usergroup =[];
while (i < 1000) {
   //console.log(i);
  user = {
  name: faker.name.findName(),
  email: faker.internet.email(),
  phone: faker.phone.phoneNumber()
};

usergroup.push(user);
    i++;
}

console.log('count:' + i);

//change to lower number above
//console.log(usergroup);

var fields = ['name', 'email', 'phone'];

var csv = json2csv({ data: usergroup, fields: fields });
 
fs.writeFile('file.csv', csv, function(err) {
  if (err) throw err;
  console.log('file saved');
});
