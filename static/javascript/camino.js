
const helpModal = bootstrap.Modal('#help-dialog')
function showHelpPopup(url) {
    $.get(url, function(data) {
        $('#help-dialog-contents').html(data);
        helpModal.show();
    });
}