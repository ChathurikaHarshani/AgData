// Custom tooltip for map identifier
L.Control.MapIdentifier = L.Control.extend({
  onAdd: function(map) {
    var div = L.DomUtil.create('div');
    div.innerHTML = '<b>Map Identifier</b>';
    return div;
  },
  onRemove: function(map) {
    // Nothing to do here
  }
});

L.control.mapIdentifier = function(opts) {
  return new L.Control.MapIdentifier(opts);
};

L.control.mapIdentifier({ position: 'topright' }).addTo(map);
