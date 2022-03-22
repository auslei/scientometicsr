// handles keypress from data_filters, make sure id matches below
$(document).keyup(function(event) {
  if ((event.keyCode == 13)) {
    if($("#data_filter-search").is(":focus")) {
      Shiny.onInputChange("data_filter-search_keypressed", Math.random());
    }
    console.log($("#data_filter-search").is(":focus"))
    console.log($("#data_filter-search").val())
  }
});