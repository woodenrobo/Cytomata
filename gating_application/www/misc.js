$(document).on('shown.bs.modal', function(x) {
    $(x.target).find('input[type=\"text\"]:first').focus();
});

$(document).on('keypress', '.modal-body input[type=\"text\"]', function(e) {
    if (e.which === 13 || e.keyCode === 13) {
      e.preventDefault();
      $(this).closest('.modal').find('.modal-footer .btn-primary').click();
    }
});