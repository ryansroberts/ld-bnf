
!function ($) {

  "use strict";

  function isConsideredHidden( $element ) {
    var hasNoSize = $element.height() === 0 && $element.width() === 0;
    var hasNoContent = $element.html() === '';


    if (hasNoSize && hasNoContent) {
        $element = $element.parent();
    }

    return $element.is(':hidden');
  }


 /* REMOTE LOAD PUBLIC CLASS DEFINITION
  * ============================== */

  var RemoteLoad = function (element, options) {
      this.$element = $(element);
      this.options = options;
  }

  RemoteLoad.prototype = {

      load: function() {
          if ( this.options.loaded ) {
            return;
          }

          if ( isConsideredHidden( this.$element ) ) {
              setTimeout( $.proxy(this.load, this), 50);

              return;
          }

          if ( this.options.processing ) {
            return;
          }

          var url = this.$element.attr('href');

          $.ajax({
                  url: url,
                  type: "GET",
                  dataType: "html",
                  contentType: "application/json; charset=utf-8"
              })
              .done($.proxy(this.done, this))
              .fail($.proxy(this.fail, this));

          this.options.processing = true;
      }

    , done: function( data, textStatus, jqXHR ) {
          this.options.loaded = true;

          this.updateTarget( data );

          this.$element.attr('href', this.$element.data('target'));

          this.complete( 'complete', textStatus, data );

          this.$element.logToLogger( 'complete', jqXHR.status, textStatus, jqXHR.responseText , data );
      }

    , fail: function( jqXHR, textStatus, errorThrown ) {
          this.complete( 'fail', textStatus, jqXHR.responseText );

          this.$element.logToLogger( 'error', jqXHR.status, textStatus, jqXHR.responseText );
      }

    , updateTarget: function( data ) {
        if (this.options.target) {
          var $target = $(this.options.target)
            , $data = $(data);

          $target.data('data', data);

          if (this.options.filter) {
              var $filtered = $data.filter(this.options.filter);

              if ($filtered.is(this.options.filter)) {
                $data = $filtered;
              }
          }

          $target.html( $data );

          $target.trigger( $.Event( 'update', { response: data } ) );
        }
      }

    , complete: function( type, textStatus, data ) {
        var option = textStatus.toLowerCase();

        this.options[option] && this.options[option]( textStatus, data );
        this.options[type] && this.options[type]( textStatus, data );
      }

  };


 /* REMOTE LOAD PLUGIN DEFINITION
  * ======================== */

  var remoteload = $.fn.remoteload

  $.fn.remoteload = function (option) {
      return this.each(function () {
          var $this = $(this)
            , data = $this.data('remoteload')
            , options = $.extend({}, $.fn.remoteload.defaults, $this.data(), options);

          if (!data) $this.data('remoteload', (data = new RemoteLoad(this, options)));

          data.load();
      })
  }

  $.fn.remoteload.defaults = {
  }

  $.fn.remoteload.Constructor = RemoteLoad;


 /* REMOTE LOAD NO CONFLICT
  * ================== */

  $.fn.remoteload.noConflict = function () {
      $.fn.remoteload = old;

      return this;
  }


 /* REMOTE LOAD DATA-API
  * =============== */

  function setup() {
      $('[data-action="load"]')
          .filter(function(){
              var $this = $(this);
              return $this.data('target') !== $this.attr('href');
          })
          .each(function() {
              $(this).remoteload();
          });
  }

  $(window).on('load', function()
  {
      setup();


      $(document).ajaxComplete(setup);
  });

}(window.jQuery);
