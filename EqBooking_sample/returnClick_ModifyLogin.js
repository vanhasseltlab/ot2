$(document).keyup(function(event) {
    if ($("#password_login_manage").is(":focus") && (event.key == "Enter")) {
        $("#login_book_manage").click();
    }
});