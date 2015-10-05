module.exports = function( grunt ) {

    'use strict';

    var config = {
      pkg: grunt.file.readJSON( './package.json' ),
      env: process.env
    };

    // load task configs
    (function loadTaskOptions( cwd, config ) {
        var path = require( 'path' );

        grunt.file.expand( { cwd: cwd }, '*' )
          .forEach(function( option ) {
            var key = option.replace( /\.js$/, '' );
            var filepath = path.join( cwd, option );

            config[ key ] = grunt.file.readJSON( filepath ); //( config );
          });
    })( './tasks/options/', config );

    grunt.initConfig( config );

    // load grunt tasks
    require( 'load-grunt-tasks' )( grunt );

    // local tasks
    grunt.loadTasks( 'tasks' );



    /*
     * Task definitions
     */

    // clean
//  grunt.registerTask('clean'             , [ 'clean' ]);


    grunt.registerTask('build:html'       , [ 'jsonld2html:aToZ', 'jsonld2html:drug', 'jsonld2html:drugClass', 'jsonld2html:clinicalMedicinalProductInformation', 'jsonld2html:medicinalForm' ]);

    // build
    grunt.registerTask('build'            , [ /*'ttl2jsonld:drug', */ 'build:html', 'copy:dist' ]);
    grunt.registerTask('rebuild'          , [ 'clean', 'build' ]);

    // preview
    grunt.registerTask('serve'            , [ 'connect:www' ]);

    // auto build
    grunt.registerTask('default'           , [ 'build' ]);

};
