$(document).keyup(function(event) {
    if ($("#password_login").is(":focus") && (event.key == "Enter")) {
        $("#login_book").click();
    }
});