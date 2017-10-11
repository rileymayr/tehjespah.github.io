$(function() {
  var a = function() {
    var b = $(window).scrollTop();
	var d = $("#scroller-anchor").offset().top;
    var c=$("#top");
    if (b>d) {
      c.css({position:"fixed",top:"0px"})
    } else {
      if (b<=d) {
        c.css({position:"relative",top:"0px"})
      }
    }
  };
  $(window).scroll(a);a()
});
