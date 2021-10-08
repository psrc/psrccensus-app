window.onload = function() {

$('#ctrlBtn').on('click', function(){
  $('.background').toggleClass('background-invisible')
  $('.visual-section').toggleClass('remove-margin')
});

$('#geog_type').on('input change', function() {
  if($(this).val() == 'tract') {
    $('input[name="vis_type"][value="graph"]').prop('disabled', true);
  } else {
    $('input[name="vis_type"][value="graph"]').prop('disabled', false);
  }
});

}

