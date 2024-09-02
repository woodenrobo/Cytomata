// scatter_plot.js
var svg, x, y, polygon, polyline;

r2d3.onRender(function(data, svg, width, height, options) {
  svg.selectAll("*").remove(); // Clear previous content

  var margin = {top: 20, right: 20, bottom: 30, left: 40};
  var innerWidth = width - margin.left - margin.right;
  var innerHeight = height - margin.top - margin.bottom;

  x = d3.scaleLinear().range([0, innerWidth]).domain(data.x_range);
  y = d3.scaleLinear().range([innerHeight, 0]).domain(data.y_range);

  var g = svg.append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  // Add X and Y axes
  g.append("g")
      .attr("transform", "translate(0," + innerHeight + ")")
      .call(d3.axisBottom(x));

  g.append("g")
      .call(d3.axisLeft(y));

  // Plot points
  g.selectAll(".point")
    .data(data.points)
    .enter().append("circle")
    .attr("class", "point")
    .attr("cx", d => x(d.x))
    .attr("cy", d => y(d.y))
    .attr("r", 2)
    .style("fill", "steelblue");

  polygon = [];
  polyline = g.append("path")
      .attr("class", "polygon")
      .style("display", "none");

  svg.on("click", function() {
    if (options.mode !== "polygon") return;
    
    var coords = d3.mouse(this);
    polygon.push([coords[0] - margin.left, coords[1] - margin.top]);
    updatePolygon();
    
    Shiny.setInputValue("polygon_coords", {
      x: polygon.map(d => x.invert(d[0])), 
      y: polygon.map(d => y.invert(d[1]))
    });
  });

  svg.on("dblclick", function() {
    if (options.mode !== "polygon") return;
    polygon = [];
    updatePolygon();
    Shiny.setInputValue("polygon_coords", null);
  });

  function updatePolygon() {
    if (polygon.length > 0) {
      polyline
        .style("display", null)
        .attr("d", "M" + polygon.join("L") + "Z");
    } else {
      polyline.style("display", "none");
    }
  }
});

Shiny.addCustomMessageHandler("update_mode", function(mode) {
  r2d3.options.mode = mode;
});

Shiny.addCustomMessageHandler("reset_selection", function(dummy) {
  polygon = [];
  updatePolygon();
  Shiny.setInputValue("polygon_coords", null);
});