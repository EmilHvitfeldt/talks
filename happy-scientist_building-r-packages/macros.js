remark.macros.scale = function(w) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + w + '" />';
};

remark.macros.scale2 = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};
