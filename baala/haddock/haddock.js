// Haddock documents of Koshucode

var Haddock = (function ($) {

    var packages =
        [ 'subtext', 'base', 'syntax', 'data', 'core',
          'writer', 'rop-flat', 'rop-nested', 'rop-cox', 'cop',
          'calculator', 'toolkit' ];

    function init () {
        packages.forEach(function (name){
            $('.buttons').append(
                '<div title="Show '
                    + name + ' package" class="button pkg-'
                    + name + '" onclick="Haddock.showDoc(\''
                    + name + '\')">' + name + '</div>');
        });
        showDoc('core');
    }

    function showDoc (name) {
        var target = $('#' + name);

        // read document
        if (target.length === 0) {
            //console.log('load ' + name);
            $('.docs').append('<iframe frameborder="0" scrolling="yes" class="doc" id="'
                              + name + '" src="' + prefix(name) + '/koshucode-baala-'
                              + name + '/index.html"></iframe>');
            calcHeight();
            target = $('#' + name);
        }

        // change z-index
        $('.doc').css('z-index', 10);
        target.css('z-index', 20);

        // select button
        $('.button').removeClass('selected');
        $('.button.pkg-' + name).addClass('selected');
    }

    function prefix (name) {
        return '../' + name + '/dist/doc/html';
        //return 'doc';
    }

    function calcHeight () {
        var doc = $('iframe');
        var sec = $('.section');
        var ht  = $(window).height() - sec.offset().top - 20;
        doc.attr('height', ht);
    }

    function showSource (pkg, dir, href) {
        $('.source').attr('src', prefix(pkg) + '/' + dir + '/' + href).show();
    }

    return {
        init: init,
        showDoc: showDoc,
        showSource: showSource
    };
})(jQuery);

