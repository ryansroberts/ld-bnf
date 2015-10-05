var Url = require('url');
var Util = require('util');
var Path = require('path');
var Stardog = require('stardog');
var RSVP = require('RSVP');
var Promise = RSVP.Promise;
var iri = require('iri');

var defaults = {

  "commit": "824b345",        // Commit hash to publish

  "dest": "./dist/stardog/",  // where to put output

  // providence
  "provURI": "http://ld.nice.org.uk/prov/commit#",

  // stardog
  "server": "192.168.99.100", // Stardog server http endpoint
  "port": 5820,
  "dbname": "nice",           // Stardog database
  "username": "admin",
  "password": "admin",
  "secure": false,             // use https

  // Remote json ld context to use
  contexts: [
    "http://192.168.99.100/ns/qualitystandard.jsonld",
    "http://192.168.99.100/ns/qualitystandard/agegroup.jsonld",
    "http://192.168.99.100/ns/qualitystandard/conditiondisease.jsonld",
    "http://192.168.99.100/ns/qualitystandard/setting.jsonld",
    "http://192.168.99.100/ns/qualitystandard/servicearea.jsonld",
    "http://192.168.99.100/ns/prov.jsonld",
    "http://192.168.99.100/ns/owl.jsonld",
    "http://192.168.99.100/ns/dcterms.jsonld"
  ],

  // Property path to include in elastic record
  propertyPaths: [
    "<http://ld.nice.org.uk/ns/qualitystandard#setting>",
    "<http://ld.nice.org.uk/ns/qualitystandard#targetPopulation>",
    "<http://ld.nice.org.uk/ns/qualitystandard#serviceArea>",
    "<http://purl.org/dc/terms/title>",
    "<http://purl.org/dc/terms/abstract>"
  ]
};

module.exports = function( grunt ) {

  grunt.registerMultiTask( 'freya', 'Renders RDF data to framed JSON-LD', function() {
    var done = this.async();

    var options = this.options( defaults );
        if ( options.port ) options.port = options.port * 1;
        if ( options.port === 443 ) options.secure = true;

    var stardogUri = ( options.secure ? "https://" : "http://" ) + options.server +
                    ( options.port && options.port !== 80 && options.port !== 443 ? ':' + options.port : '' );

    var stardog = new Stardog.Connection();
        stardog.setEndpoint( stardogUri );
        stardog.setCredentials( options.username, options.password );
/*
    var commit = Url.parse( Util.format( '%s%s', options.provURI, options.commit ) );

    var contexts = options.contexts;

    var propertyPaths = getIRIs( options.propertyPaths );

    var contexts = contexts.map(function( x ) { return Util.format( ' "%s" ', x ); }).join(',\n');
    var context = JSON.parse(Util.format('{\
  "@context": [\
    { "@language" : "en" },\
    %s,\
    { "resource" : "http://ld.nice.org.uk/resource#" }\
  ]\
}', contexts ) );
*/
//  console.dir( context );

    return getResources( stardog, options )
//    .then( getEntityForResources( commit, options ) )
//    .then( getSubGraphForResources( propertyPaths, options ) )
//    .then( getResourceFromType( 'http://www.w3.org/2002/07/owl#NamedIndividual', options ) )
      .then(function( r ) {
        console.dir( r );
        return r;
      })
      .then( filterEmptyOrInvalidResources )
//    .then( getCompactedJsonLD( context ) )
//    .then( publishToElasticSearch )
      .then(function( resources ) {
        resources.forEach(function( json ) {
          var id = json._id
          grunt.log.ok( Util.format( 'Writing ld for %s', id ) );

          var m = /([^:]*):(.*)/gmi.exec( id );

          var filename = m[2] + '.jsonld';
          var filepath = Path.join( options.dest, filename );
          var content = JSON.stringify( json, null, '\t' );

          grunt.file.write( filepath, content );
        });
      })
      .catch(function( err ) {
        console.error( err );
      });

  });







  // sparql

  var getResourcesQuery = '\
prefix prov:  <http://www.w3.org/ns/prov#>\
\
select ?resource\
from <http://ld.nice.org.uk/ns>\
from <http://ld.nice.org.uk/prov>\
from <http://ld.nice.org.uk/>\
where {\
?s prov:specializationOf ?resource .\
}';

  var getEntityForResourceQuery = '\
prefix prov: <http://www.w3.org/ns/prov#>\
prefix niceprov: <http://ld.nice.org.uk/prov>\
prefix compilation: <http://ld.nice.org.uk/ns/compilation#>\
prefix nice: <http://ld.nice.org.uk/>\
\
select *\
  from niceprov:\
  from nice:\
where {\
  ?entity prov:specializationOf %s.\
  ?entity ^prov:uses ?commit .\
  ?commit a compilation:Commit .\
  {\
    select ?commit (count(?nextCommit) as ?count)\
    where {\
      %s prov:informedBy* ?nextCommit .\
      ?nextCommit prov:informedBy* ?commit .\
    }\
    group by ?commit\
  }\
}\
order by desc(?count)\
LIMIT 1';

  var getSubGraphQuery = '\
prefix prov: <http://www.w3.org/ns/prov#>\
prefix niceprov: <http://ld.nice.org.uk/prov>\
prefix compilation: <http://ld.nice.org.uk/ns/compilation#>\
prefix nice: <http://ld.nice.org.uk/>\
prefix dcterms: <http://purl.org/dc/terms/>\
\
construct {\
  %s a ?t .\
  %s prov:specializationOf ?r .\
  %s prov:alternateOf ?alt .\
  %s\
}\
  from <http://ld.nice.org.uk/ns>\
  from <http://ld.nice.org.uk/prov>\
  from <http://ld.nice.org.uk/>\
where {\
  %s a ?t .\
  %s prov:specializationOf ?r .\
  optional { %s prov:alternateOf ?alt .  } .\
  %s\
}';







  // helpers

  function getResources( stardog, options ) {
    return new Promise(function( resolve, reject ) {
      var query = getResourcesQuery;

      grunt.verbose.ok( query );

      stardog.query(
        {
          database: options.dbname,
          query: query
        },
        onComplete
      );

      function onComplete( data ) {
        if ( !data || !data.results ) {
          return reject( new Error( 'no data returned' ) );
        }

        resolve( data.results );
      }
    });
  }

  function getEntityForResources( commitUri, options ) {
    return function( resources ) {
      return RSVP.all( resources.map( getEntityForResource( commitUri, options ) ) ).then( flattenPromiseResults );
    };
  }

  function getEntityForResource( commitUri, options ) {
    return function( resourceUri ) {
      return new Promise(function( resolve, reject ) {
        var query = Util.format( getEntityForResourceQuery, resourceUri, commitUri );

        grunt.verbose.ok( query );

        stardog.query(
          {
            database: options.dbname,
            query: query
          },
          onComplete
        );

        function onComplete( data ) {
          if ( !data || !data.results ) {
            return reject();
          }

          resolve( data.results );
        }
      });
    }
  }

  function getSubGraphForResources( propertyPaths, options ) {
    return function( resources ) {
      return RSVP.all( resources.map( getSubGraphForEachResource( propertyPaths, options ) ) ).then( flattenPromiseResults );
    };
  }

  function getSubGraphForEachResource( propertyPaths, options ) {
    return function( resource ) {
      return new Promise(function( resolve, reject ) {
        var query = Util.format( getSubGraphQuery, resource, resource, resource, resource, resource, resource, resource , resource );

        grunt.verbose.ok( query );

        stardog.query(
          {
            baseURI: 'http://ld.nice.org.uk/',
            database: options.dbname,
            query: query,
            construct: propertyPaths,
            clause: propertyPaths
          },
          onComplete
        );

        function onComplete( data ) {
          if ( !data || !data.results ) {
            return reject();
          }

          resolve( data.results );
        }
      });
    };
  }

  function flattenPromiseResults( x ) {
    return distinctInArray( x.reduce( flattenArray ) );
  }

  function distinctInArray( x ) {
     var u = {}, a = [];
     for ( var i = 0, l = x.length; i < l; ++i){
        if ( u.hasOwnProperty( x[ i ] ) ) continue;
        a.push( x[ i ]);
        u[ x[ i ] ] = 1;
     }
     return a;
  }

  function flattenArray( a, b ) {
    return Array.prototype.concat.call( a, b );
  }

  function filterEmptyOrInvalidResources( resources ) {
    return resources.filter( isJsonLD );
  }

  function getIRIs( x ) {
    return Util.isArray( x ) ? x.map( asIRI ).filter( isDefined ) : [];
  }

  function isJsonLD( x ) {
    return isDefined( x ) && isDefined( x._id );
  }

  function isDefined( x ) {
    return undefined != x;
  }

  function asIRI( x ) {
    return ( x && iri.fromURI( x ) ) || undefined;
  }
};
