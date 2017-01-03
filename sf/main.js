function toggleDisplay(id)
{
   var elt = document.getElementById(id);
   if (elt.style.display == 'none') {
     elt.style.display = 'block';
   } else {
     elt.style.display = 'none';
   }
}
function hideAll(cls)
{
  var testClass = new RegExp("(^|s)" + cls + "(s|$)");
  var tag = tag || "*";
  var elements = document.getElementsByTagName("div");
  var current;
  var length = elements.length;
  for(var i=0; i<length; i++){
    current = elements[i];
    if(testClass.test(current.className)) {
      current.style.display = 'none';
    }
  }
}

var slideSelector = 'h1.libtitle, h1.section, h2.section, h3.section, .quiz';

var slideMode = false;

function slideNavigate(direction) {

    function slideNumber(s) {
        if (!s) return null;
        var match = s.match(/slide-(.*)/);
        if (match && match.length != 0) {
            return parseInt(match[1]);
        }
        return null;
    }

    var curSlide = slideNumber(location.hash);
    var lastSlide = slideNumber($('.slide').last().attr('id'));
    var nextSlide;

    /* We change the id of each slide element when the page loads, and
     * then switch between slides based on the current hash. This is
     * not entirely optimal, and can probably be made better.
     * http://www.appelsiini.net/projects/viewport seems to be a nice choice.
     */

    if (direction == 'left') {
        if (curSlide != null) {
            if (curSlide > 0) {
                nextSlide = curSlide - 1;
            } else {
                nextSlide = lastSlide;
            }
        } else {
            nextSlide = 0;
        }
    } else if (direction == 'right') {
        if (curSlide != null && curSlide < lastSlide) {
            nextSlide = curSlide + 1;
        } else {
            nextSlide = 0;
        }
    }

    location.hash = '#slide-' + nextSlide;

    return false;
};

function refreshHash() {
    // Force the browser to scroll back to the right object
    var t = location.hash;
    location.hash = '';
    location.hash = t;
}

function slideActivate() {
    $('.slide').each(function (i, elt) {
        if (i > 0) $(elt).css('margin-top', $(window).height());
        $(elt).css('height', '20px');
    });
    $('#main').css('padding-bottom', $(window).height());
    slideMode = true;
    if (location.hash) {
        refreshHash();
    } else {
        location.hash = '#slide-0';
    }
}

function slideDeactivate() {
    $('.slide').each(function (i, elt) {
        $(elt).css('margin-top', 0);
        $(elt).css('height', 0);
    });
    $('#main').css('padding-bottom', 0);
    refreshHash();
    slideMode = false;
}

$(document).keydown(function (event) {
    if (slideMode) {
        if (event.keyCode == 37) {
            slideNavigate('left');
        } else if (event.keyCode == 39) {
            slideNavigate('right');
        } else if (event.keyCode == 27) { // escape
            slideDeactivate();
        } else return true;
    } else {
        if (event.keyCode == 37 || event.keyCode == 39) {
            slideActivate();
            return false;
        } else {
            return true;
        }
    }
});

$(document).ready(function () {
    hideAll('proofscript');
    $(slideSelector).each(function (i, elt) {
        var mark = '<div class="slide" id="slide-' + i + '" />';
        $(mark).insertBefore($(elt));
    });
    if (location.hash) {
        slideActivate();
    }
});
