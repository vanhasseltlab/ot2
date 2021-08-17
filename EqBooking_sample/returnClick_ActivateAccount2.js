$(document).keyup(function(event) {
    if ($("#password").is(":focus") && (event.key == "Enter")) {
        $("#new_pass").click();
    }
});