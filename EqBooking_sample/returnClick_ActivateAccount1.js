$(document).keyup(function(event) {
    if ($("#activation_user").is(":focus") && (event.key == "Enter")) {
        $("#activate_confirm").click();
    }
});