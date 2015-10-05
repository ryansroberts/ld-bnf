var Stardog = require('stardog');
var RSVP = require('RSVP');
var Promise = RSVP.Promise;
var N3 = require('n3');
var N3Util = N3.Util;
var sparql = require('sparqljs');
var SparqlGenerator = sparql.Generator;

var defaults = {

  // stardog
  "server": "192.168.99.100", // Stardog server http endpoint
  "port": 5820,
  "dbname": "nice",           // Stardog database
  "username": "admin",
  "password": "admin",
  "secure": false             // use https
};

module.exports = function( grunt ) {

  grunt.registerMultiTask( 'stardogPublish', 'publishes triples into stardog server', function() {
    var done = this.async();

    var options = this.options( defaults );
        if ( options.port ) options.port = options.port * 1;
        if ( options.port === 443 ) options.secure = true;

    var stardogUri = ( options.secure ? "https://" : "http://" ) + options.server +
                    ( options.port && options.port !== 80 && options.port !== 443 ? ':' + options.port : '' );

    var stardog = new Stardog.Connection();
        stardog.setEndpoint( stardogUri );
        stardog.setCredentials( options.username, options.password );

    var sparkqlGenerator = new SparqlGenerator();

    var query = {
      type: 'update',
      updates: []
    };
    var update = {
      updateType: 'insert',
      insert: []
    };
    query.updates.push( update );
    var insert = {
      type: 'bgp',
      triples: []
    };
    update.insert.push( insert );

    var files = Array.prototype.slice.call( this.files );
    process( new N3.Parser(), files, onData );

    function onData( data, pfx ) {
      insert.triples.push( data );

      if ( pfx ) {
        var prefixes = query.prefixes = query.prefixes || {};
        for ( var p in pfx ) {
          console.log( 'copying: ', p )
          if ( !prefixes[ p ] ) prefixes[ p ] = pfx[ p ];
        }
      }
    }

    function process( N3Parser, files, cb ) {
      if ( files.length <= 0 ) {
        grunt.log.ok( 'Generating SparQl...' );
        var sparqlQuery = sparkqlGenerator.stringify( query );
        grunt.verbose.ok( sparqlQuery );

        grunt.log.ok( 'Publishing to Stardog server...' );
        publishToStore( stardog, options, sparqlQuery ).then( done, grunt.fail.fatal );

        return;
      }

      var file = files.pop();

      console.log( 'hello...' );

      grunt.log.writeln( "Loading " + file.src[0] + "...");
      var content = grunt.file.read(file.src[0], { encoding: 'utf8' });

        grunt.log.error( content );
      N3Parser.parse( content, onParse );
            console.log( '...hello' );

      function onParse( error, data, pfx ) {
        if ( error ) {
          grunt.log.error( content );
          grunt.fail.fatal( error );
          return;
        }

        if ( data && cb ) {
          cb( data, pfx );
        }

        process( N3Parser, files, onData );
      }
    }

  });







  // helpers

  function publishToStore( stardog, options, query ) {
    return new Promise(function( resolve, reject ) {
      stardog.query({
          database: options.dbname,
          query: query
        },
        onComplete );

      function onComplete( data, response ) {
        if ( response.statusCode < 200 || response.statusCode > 299 ) {
          var err = new Error( response.statusMessage + ' [ ' + query + ' ]' );
          err.statusCode = response.statusCode;
          err.response = response;

          grunt.fail.fatal( err );

          return reject( err );
        }

        resolve( data.results || data );
      }
    });
  }
};
