var N3 = require('n3');
var N3Util = N3.Util;
var path = require('path');
var jsonld = require('jsonld');
var Parser = jsonld.promises;
var Handlebars = require('handlebars');
var deasync = require('deasync');

var defaults = {
  rdfFormat:   'N3',
  frames:      './src/frames/bnf/'
};

module.exports = function( grunt ) {

  grunt.registerMultiTask( 'ttl2jsonld', 'Renders RDF data to framed JSON-LD', function() {
    var done = this.async();

    var options = this.options( defaults );
    options.model = this.target;

    var files = this.files.slice();
    var frames = loadFrames( options.frames );

    process();

    function process() {
      if ( files.length <= 0 ) {
        done();
        return;
      }

      var file = files.pop();

      grunt.log.writeln( "Loading " + file.src[0] + "...");
      var content = grunt.file.read(file.src[0], { encoding: 'utf8' });

      jsonld.registerRDFParser( options.rdfFormat, _N3 );

      Parser
        .fromRDF( content, { format: options.rdfFormat } )
        .then( frameWith( frames, options ) )
        .then(function( json ) {
          grunt.file.write( file.dest, JSON.stringify( json, null, '\t' ) );
          grunt.log.ok( "Output written to " + file.dest );

          process();
        })
        .catch( grunt.fail.fatal );
    }

    function _N3( input ) {
      var done;
      var dataset = {};
      var parser = new N3.Parser({ format: options.rdfFormat });

      parser.parse( input, function ( error, data, prefixes ) {
        if ( error ) {
          throw error;
        }

        if ( data ) {
          var triple = {};

          // get subject
          triple.subject = { type: 'IRI', value: data.subject };

          // get predicate
          triple.predicate = { type: 'IRI', value: data.predicate };

          // get object
          if( N3Util.isIRI( data.object ) ) {
            triple.object = { type: 'IRI', value: data.object };
          } else if ( N3Util.isLiteral( data.object ) ) {
            triple.object = {
              type: 'literal',
              datatype: N3Util.getLiteralType( data.object ),
              language: N3Util.getLiteralLanguage( data.object ),
              value: N3Util.getLiteralValue( data.object )
            };
          } else {
            triple.object = { type: 'blank node', value: match[5] };
          }

          var graph = '@default';
          if ( data.context && !~data.context.indexOf('n3/contexts#default') ) {
            graph = data.context;
          }

          (dataset[ graph ] = dataset[ graph ] || []).push( triple );
        }

        done = true;
      });

      deasync.loopWhile(function(){ return !done; });

      return dataset;
    }
  });




  // helpers

  function frameWith( frames, options ) {
    var frames = frames;

    return function( doc ) {
      var type = options.model;

      var frame = frames[ type ];

      if ( !type || !frame ) {
        throw new Error( 'No type ['+ type +'] found to frame JSON-LD' );
      }

      grunt.verbose.writeln( 'Using frame:' );
      grunt.verbose.writeln( JSON.stringify( frame, null, '  ' ) );
      grunt.verbose.writeln();

      return Parser.frame( doc, frame );
    }
  }

  function getModelFromFramedDocument( doc ) {
    var graphProperty = "@graph";
    var contextProperty = "@context";

    var model = doc[ graphProperty ] || doc;
    model = model[ 0 ] || model;

    if ( model[ contextProperty ] ) {
      delete model[ contextProperty ];
    }

    return clone( model );
  }

  function clone( b ) {
    var a = {};

    // rewrite property names whilst cloning

    grunt.util._.extend( a, b );

    return a;
  }

  function loadFrames( cwd ) {
  	var files = grunt.file.expand( { cwd: cwd }, '*.json' );

    return process( files );

    function process( files, frames ) {
      frames = frames || {};

      if ( files.length <= 0 ) {
        return frames;
      }

      var filename = files.pop();
      var file = path.join( cwd, filename );
      var type = filename.replace( '.json', '' );

      grunt.verbose.writeln( "Loading frame " + file + "...");
      var content = grunt.file.read(file, { encoding: 'utf8' });

      frames[ type ] = JSON.parse( content );

      return process( files, frames );
    }
  }

};
