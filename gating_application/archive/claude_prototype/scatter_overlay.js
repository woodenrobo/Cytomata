// scatter_overlay.js
var svg, x, y, polygon, polyline;

r2d3.onRender(function(data, div, width, height, options) {
  div.selectAll("*").remove(); // Clear previous content
  
  svg = div.append("svg")
    .attr("width", data.width)
    .attr("height", data.height);

  var margin = {top: 20, right: 20, bottom: 30, left: 40};
  var innerWidth = data.width - margin.left - margin.right;
  var innerHeight = data.height - margin.top - margin.bottom;

  x = d3.scaleLinear().range([0, innerWidth]).domain(data.x_range);
  y = d3.scaleLinear().range([innerHeight, 0]).domain(data.y_range);

  var g = svg.append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  polygon = [];
  polyline = g.append("path")
      .attr("class", "polygon")
      .style("display", "none");

  if (options.polygon) {
    polygon = options.polygon.x.map((x, i) => [x(x), y(options.polygon.y[i])]);
    updatePolygon();
  }
});

function addPolygonPoint(point) {
  if (r2d3.options.mode !== "polygon") return;
  
  polygon.push([x(point.x), y(point.y)]);
  updatePolygon();
  
  Shiny.setInputValue("polygon_coords", {
    x: polygon.map(d => x.invert(d[0])), 
    y: polygon.map(d => y.invert(d[1]))
  });
}

function updatePolygon() {
  if (polygon.length > 0) {
    polyline
      .style("display", null)
      .attr("d", "M" + polygon.join("L") + "Z");
  } else {
    polyline.style("display", "none");
  }
}

Shiny.addCustomMessageHandler("add_polygon_point", addPolygonPoint);

Shiny.addCustomMessageHandler("update_hover_point", function(hover_point) {
  if (polygon.length > 0) {
    var hoverCoords = [x(hover_point.x), y(hover_point.y)];
    var polygonPath = "M" + polygon.join("L") + "L" + hoverCoords.join(",") + "Z";
    polyline.attr("d", polygonPath);
  }
});

svg.on("dblclick", function() {
  if (r2d3.options.mode !== "polygon") return;
  polygon = [];
  updatePolygon();
  Shiny.setInputValue("polygon_coords", null);
});