// scatter_overlay.js
var svg, x, y, currentGate, gates = [];

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

  currentGate = null;

  // Render existing gates
  if (options.gates) {
    gates = options.gates;
    renderGates();
  }
});

function startGate(type, point) {
  switch(type) {
    case "Rectangle":
      currentGate = {type: type, x1: point.x, y1: point.y, x2: point.x, y2: point.y};
      break;
    case "Polygon":
      currentGate = {type: type, points: [point]};
      break;
    case "Ellipse":
      currentGate = {type: type, cx: point.x, cy: point.y, rx: 0, ry: 0};
      break;
    case "Quadrant":
      currentGate = {type: type, x: point.x, y: point.y};
      break;
  }
  renderCurrentGate();
}

function updateGate(point) {
  if (!currentGate) return;

  switch(currentGate.type) {
    case "Rectangle":
      currentGate.x2 = point.x;
      currentGate.y2 = point.y;
      break;
    case "Polygon":
      currentGate.points.push(point);
      break;
    case "Ellipse":
      currentGate.rx = Math.abs(point.x - currentGate.cx);
      currentGate.ry = Math.abs(point.y - currentGate.cy);
      break;
    case "Quadrant":
      // Quadrant doesn't need updating
      break;
  }
  renderCurrentGate();
}

function endGate() {
  if (!currentGate) return;

  gates.push(currentGate);
  Shiny.setInputValue("gate_coords", currentGate);
  currentGate = null;
  renderGates();
}

function renderCurrentGate() {
  svg.selectAll(".current-gate").remove();
  if (!currentGate) return;

  switch(currentGate.type) {
    case "Rectangle":
      svg.append("rect")
        .attr("class", "gate current-gate")
        .attr("x", x(Math.min(currentGate.x1, currentGate.x2)))
        .attr("y", y(Math.max(currentGate.y1, currentGate.y2)))
        .attr("width", Math.abs(x(currentGate.x2) - x(currentGate.x1)))
        .attr("height", Math.abs(y(currentGate.y2) - y(currentGate.y1)));
      break;
    case "Polygon":
      svg.append("polygon")
        .attr("class", "gate current-gate")
        .attr("points", currentGate.points.map(p => `${x(p.x)},${y(p.y)}`).join(" "));
      break;
    case "Ellipse":
      svg.append("ellipse")
        .attr("class", "gate current-gate")
        .attr("cx", x(currentGate.cx))
        .attr("cy", y(currentGate.cy))
        .attr("rx", Math.abs(x(currentGate.cx + currentGate.rx) - x(currentGate.cx)))
        .attr("ry", Math.abs(y(currentGate.cy + currentGate.ry) - y(currentGate.cy)));
      break;
    case "Quadrant":
      svg.append("line")
        .attr("class", "gate current-gate")
        .attr("x1", x(currentGate.x))
        .attr("y1", y.range()[0])
        .attr("x2", x(currentGate.x))
        .attr("y2", y.range()[1]);
      svg.append("line")
        .attr("class", "gate current-gate")
        .attr("x1", x.range()[0])
        .attr("y1", y(currentGate.y))
        .attr("x2", x.range()[1])
        .attr("y2", y(currentGate.y));
      break;
  }
}

function renderGates() {
  svg.selectAll(".gate:not(.current-gate)").remove();
  gates.forEach((gate, index) => {
    switch(gate.type) {
      case "Rectangle":
        svg.append("rect")
          .attr("class", "gate")
          .attr("data-index", index)
          .attr("x", x(Math.min(gate.x1, gate.x2)))
          .attr("y", y(Math.max(gate.y1, gate.y2)))
          .attr("width", Math.abs(x(gate.x2) - x(gate.x1)))
          .attr("height", Math.abs(y(gate.y2) - y(gate.y1)));
        break;
      case "Polygon":
        svg.append("polygon")
          .attr("class", "gate")
          .attr("data-index", index)
          .attr("points", gate.points.map(p => `${x(p.x)},${y(p.y)}`).join(" "));
        break;
      case "Ellipse":
        svg.append("ellipse")
          .attr("class", "gate")
          .attr("data-index", index)
          .attr("cx", x(gate.cx))
          .attr("cy", y(gate.cy))
          .attr("rx", Math.abs(x(gate.cx + gate.rx) - x(gate.cx)))
          .attr("ry", Math.abs(y(gate.cy + gate.ry) - y(gate.cy)));
        break;
      case "Quadrant":
        svg.append("line")
          .attr("class", "gate")
          .attr("data-index", index)
          .attr("x1", x(gate.x))
          .attr("y1", y.range()[0])
          .attr("x2", x(gate.x))
          .attr("y2", y.range()[1]);
        svg.append("line")
          .attr("class", "gate")
          .attr("data-index", index)
          .attr("x1", x.range()[0])
          .attr("y1", y(gate.y))
          .attr("x2", x.range()[1])
          .attr("y2", y(gate.y));
        break;
    }
  });

  svg.selectAll(".gate")
    .on("click", function() {
      var index = d3.select(this).attr("data-index");
      Shiny.setInputValue("selected_gate", index);
    });
}

Shiny.addCustomMessageHandler("start_gate", function(data) {
  startGate(data.type, data.point);
});

Shiny.addCustomMessageHandler("update_gate", function(data) {
  updateGate(data.point);
});

Shiny.addCustomMessageHandler("end_gate", function() {
  endGate();
});

Shiny.addCustomMessageHandler("delete_gate", function(index) {
  gates.splice(index, 1);
  renderGates();
  Shiny.setInputValue("gates", gates);
});