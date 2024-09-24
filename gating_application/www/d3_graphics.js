

// # defining plot information container

var plotInfo = {
  x_axis_min: 0,
  x_axis_max: 0,
  x_total: 0,
  x_name: '',
  y_axis_min: 0,
  y_axis_max: 0,
  y_total: 0,
  y_name: '',
  plot_res: 0,
  d3_x_res: 0,
  d3_y_res: 0,
  cx: 0,
  cy: 0,
  svg: null,
  xScale: null,
  yScale: null,
  xAxis: null,
  yAxis: null,
  margin: {top: 20, right: 20, bottom: 30, left: 45}
};



// # defining gating information container
var gatesInfo = {
  current_gate_mode: 'off',
  detected_gates_biaxial: null,
  detected_gates_mono: null,
  gating_svg: null,
  pointerEventsEnabled: true,
  clip_gates: true
};


// # defining tree information container
var treeInfo = {
  tree_object: null,
  selectedNode: null
}

// # defining chatbot information container
var chatInfo = {
  botui: null,
  response: null
}

// #  handle plot panel margins

Shiny.addCustomMessageHandler('plot_margin', function(margins) {
  plotInfo.margin.top = margins[0];
  plotInfo.margin.right = margins[1];
  plotInfo.margin.bottom = margins[2];
  plotInfo.margin.left = margins[3];

  console.log('Received plot margins from server:', plotInfo.margin.top,  plotInfo.margin.right, plotInfo.margin.bottom,  plotInfo.margin.left);

});


// #  resize the plot container

Shiny.addCustomMessageHandler('plot_resolution', function(plot_resolution) {
  console.log('Received plot resolution from server:', plot_resolution);
  plotInfo.plot_res = plot_resolution;

  plotInfo.d3_x_res = plotInfo.plot_res - plotInfo.margin.right - plotInfo.margin.left;
  plotInfo.d3_y_res = plotInfo.plot_res - plotInfo.margin.top - plotInfo.margin.bottom;

  redrawSVG();
});


// #  handle x axis ranges

Shiny.addCustomMessageHandler('x_axis_range', function(x_range) {
  plotInfo.x_axis_min = x_range[0];
  plotInfo.x_axis_max = x_range[1];
  plotInfo.x_total = plotInfo.x_axis_max - plotInfo.x_axis_min;
  console.log('Received x axis range from server:', plotInfo.x_axis_min, plotInfo.x_axis_max);
});


// #  handle y axis ranges

Shiny.addCustomMessageHandler('y_axis_range', function(y_range) {
  plotInfo.y_axis_min = y_range[0];
  plotInfo.y_axis_max = y_range[1];
  plotInfo.y_total = plotInfo.y_axis_max - plotInfo.y_axis_min;
  console.log('Received y axis range from server:', plotInfo.y_axis_min, plotInfo.y_axis_max);
});


// #  handle x axis names
Shiny.addCustomMessageHandler('x_channel_select', function(x_name) {
  plotInfo.x_name = x_name;
  console.log('Received x axis name from server:', plotInfo.x_name);
});

// #  handle y axis names
Shiny.addCustomMessageHandler('y_channel_select', function(y_name) {
  plotInfo.y_name = y_name;
  console.log('Received y axis name from server:', plotInfo.y_name);
});

// setting: whether to clip the gates at the axis
Shiny.addCustomMessageHandler("clip_gates", function(clip_gates) {
  gatesInfo.clip_gates = clip_gates
});


// # JS code for gating mode switch

Shiny.addCustomMessageHandler('gate_mode', function(gate_mode) {
  console.log('Received gate mode from server:', gate_mode);
  gatesInfo.current_gate_mode = gate_mode;

});


// change handles styling when the gate is selected in tree
function updateGateHighlight(selectedNode) {
  var selectedNodeName = "/" + selectedNode
  console.log("Selected Node Name:", selectedNodeName);

  d3.selectAll('.handle')
    .style('fill', function() {
      var parentNode = d3.select(this).node().parentNode;
      if (parentNode) {
        var gateName = d3.select(parentNode).attr('data-gate-name');
        console.log('gateName:', gateName, 'selectedNodeName:', selectedNodeName);
        return gateName === selectedNodeName ? '#47c9f5' : 'white';
      }
      return 'white'; // Default color if parent node doesn't exist
    });
}


// raise selected gate "g" object
function raiseSelectedGate(selectedNode) {
  var selectedNodeName = "/" + selectedNode
  
  // Select all gateGroup elements
  d3.selectAll('.gate-group')
  .filter(function() {
    // Filter elements based on the data-gate-name attribute
    return d3.select(this).attr('data-gate-name') === selectedNodeName;
  })
  .raise(); // Raise the filtered element to the front
  console.log("gate-group should have been raised", selectedNodeName);
}


// Handler for plot updates
Shiny.addCustomMessageHandler('plot_done', function(message) {
  console.log('Received plot_done message');
  redrawSVG();
  redrawGatingSVG();

});


function redrawSVG() {

  // Remove existing SVG if it exists
  if( plotInfo.svg != null ){
    plotInfo.svg.remove();
  }
  
  const width = plotInfo.plot_res - plotInfo.margin.left - plotInfo.margin.right;
  const height = plotInfo.plot_res - plotInfo.margin.top - plotInfo.margin.bottom;

  // Create SVG
  plotInfo.svg = d3.select('#d3_output')
    .append('svg')
    .attr('width', plotInfo.plot_res)
    .attr('height', plotInfo.plot_res)
    .style('position', 'absolute')
    .style('z-index', '1000')
    .append('g')
    .attr('transform', `translate(${plotInfo.margin.left},${plotInfo.margin.top})`);


  console.log('SVG reinitialized');

  // Create scales
  plotInfo.xScale = d3.scaleLinear()
    .domain([plotInfo.x_axis_min, plotInfo.x_axis_max])
    .range([0, width]);

  plotInfo.yScale = d3.scaleLinear()
    .domain([plotInfo.y_axis_min, plotInfo.y_axis_max])
    .range([height, 0]);

  // Create axes
  plotInfo.xAxis = d3.axisBottom(plotInfo.xScale);
  plotInfo.yAxis = d3.axisLeft(plotInfo.yScale);

  // Append axes to SVG
  if (plotInfo.x_axis_max > 100000 || plotInfo.x_axis_min < -100000) {
    plotInfo.xAxis.tickFormat(d3.format('.0e'));
  }
  if (plotInfo.y_axis_max > 100000 || plotInfo.y_axis_min < -100000) {
    plotInfo.yAxis.tickFormat(d3.format('.0e'));
  }

  plotInfo.svg.append('g')
    .attr('class', 'x-axis')
    .attr('transform', `translate(0,${height})`)
    .call(plotInfo.xAxis);

  plotInfo.svg.append('g')
    .attr('class', 'y-axis')
    .call(plotInfo.yAxis);

  console.log('Axes redrawn');
}


function redrawGatingSVG() {
  // Remove existing SVG if it exists
  if ( gatesInfo.gating_svg != null ){
    gatesInfo.gating_svg.remove();
  }
  
  const handleRadius = 5;


  // Create SVG
  gatesInfo.gating_svg = d3.select('#d3_output_gates')
    .append('svg')
    .attr('width', plotInfo.d3_x_res)
    .attr('height', plotInfo.d3_y_res)
    .style('position', 'absolute')
    .style('z-index', '2000');


  console.log('GATING SVG reinitialized');

  if (gatesInfo.detected_gates_biaxial.names != null) {

    for (i in gatesInfo.detected_gates_biaxial.names) {
      console.log('Detected gate:', gatesInfo.detected_gates_biaxial.names[i]);
      console.log('Detected gate type:', gatesInfo.detected_gates_biaxial.types[i]);
      console.log('Detected gate axes1:', gatesInfo.detected_gates_biaxial.axis1[i]);
      console.log('Detected gate axes2:', gatesInfo.detected_gates_biaxial.axis2[i]);
      console.log('Detected gate coordinates:', gatesInfo.detected_gates_biaxial.coords[i]);

      // Pre-filter for rectangle gates
      const rectangleGates = gatesInfo.detected_gates_biaxial.names.reduce((acc, name, index) => {
        if (gatesInfo.detected_gates_biaxial.types[index] === "rectangleGate") {
          acc.push({
            name: name,
            axis1: gatesInfo.detected_gates_biaxial.axis1[index],
            axis2: gatesInfo.detected_gates_biaxial.axis2[index],
            coords: gatesInfo.detected_gates_biaxial.coords[index]
          });
        }
        return acc;
      }, []);

      if (gatesInfo.detected_gates_biaxial.types[i] === "rectangleGate") {
        let sw, nw, ne, se;
        
        if (gatesInfo.detected_gates_biaxial.axis1[i] == plotInfo.x_name && gatesInfo.detected_gates_biaxial.axis2[i] == plotInfo.y_name) {
          sw = {x: gatesInfo.detected_gates_biaxial.coords[i][0][0], y: gatesInfo.detected_gates_biaxial.coords[i][0][1]};
          nw = {x: gatesInfo.detected_gates_biaxial.coords[i][0][0], y: gatesInfo.detected_gates_biaxial.coords[i][1][1]};
          ne = {x: gatesInfo.detected_gates_biaxial.coords[i][1][0], y: gatesInfo.detected_gates_biaxial.coords[i][1][1]};
          se = {x: gatesInfo.detected_gates_biaxial.coords[i][1][0], y: gatesInfo.detected_gates_biaxial.coords[i][0][1]};
        } else if (gatesInfo.detected_gates_biaxial.axis1[i] == plotInfo.y_name && gatesInfo.detected_gates_biaxial.axis2[i] == plotInfo.x_name) {
          sw = {x: gatesInfo.detected_gates_biaxial.coords[i][0][1], y: gatesInfo.detected_gates_biaxial.coords[i][0][0]};
          nw = {x: gatesInfo.detected_gates_biaxial.coords[i][1][1], y: gatesInfo.detected_gates_biaxial.coords[i][0][0]};
          ne = {x: gatesInfo.detected_gates_biaxial.coords[i][1][1], y: gatesInfo.detected_gates_biaxial.coords[i][1][0]};
          se = {x: gatesInfo.detected_gates_biaxial.coords[i][0][1], y: gatesInfo.detected_gates_biaxial.coords[i][1][0]};
        }

        const x = plotInfo.xScale(sw.x);
        const y = plotInfo.yScale(ne.y);
        const width = plotInfo.xScale(ne.x) - plotInfo.xScale(sw.x);
        const height = plotInfo.yScale(sw.y) - plotInfo.yScale(ne.y);

        // Create a group for the gate
        const gateGroup = gatesInfo.gating_svg.append('g')
          .attr('class', 'gate-group')
          .attr('data-gate-name', gatesInfo.detected_gates_biaxial.names[i]);

        // Append the rectangle to the group
        const box = gateGroup.append('rect')
          .attr('x', x)
          .attr('y', y)
          .attr('width', width)
          .attr('height', height)
          .attr('class', 'box')
          .attr('id', `gate-${i}`)
          .style('cursor', 'pointer')
          .style('fill', 'none')
          .style('stroke', 'black')
          .style('stroke-width', 3)
          .call(d3.drag()
            .on('start', dragStart)
            .on('drag', dragging)
            .on('end', dragEnd)
          );

        box.data([
          {startX: +box.attr('x')},
          {startY: +box.attr('y')}
        ]);

        box.attr('data-gate-name', gatesInfo.detected_gates_biaxial.names[i]);

        const handles = gateGroup.selectAll(`circle.handle-${i}`)
          .data([
            { x: x, y: y, cursor: 'nw-resize' },
            { x: x + width, y: y, cursor: 'ne-resize' },
            { x: x, y: y + height, cursor: 'sw-resize' },
            { x: x + width, y: y + height, cursor: 'se-resize' }
          ])
          .enter().append('circle')
          .attr('class', `handle handle-${i}`)
          .attr('cx', d => d.x)
          .attr('cy', d => d.y)
          .attr('r', handleRadius)
          .style('cursor', d => d.cursor)
          .call(d3.drag()
            .on('start', handleDragStart)
            .on('drag', handleDragging)
            .on('end', handleDragEnd)
          );


        // Add semi-transparent background for text
        gateGroup.append('rect')
          .attr('x', x + width / 2)
          .attr('y', y + height / 2)
          .attr('width', 80)  // Adjust as needed
          .attr('height', 20) // Adjust as needed
          .attr('fill', 'rgba(255, 255, 255, 0.7)')
          .attr('transform', 'translate(-40, -10)')  // Center the background
          .attr('pointer-events', 'none');

        // Add text overlay
        gateGroup.append('text')
          .attr('x', x + width / 2)
          .attr('y', y + height / 2)
          .attr('text-anchor', 'middle')
          .attr('dominant-baseline', 'central')
          .attr('fill', 'black')
          .attr('font-size', '12px')
          .attr('pointer-events', 'none')
          .text(gatesInfo.detected_gates_biaxial.names[i].split('/').pop());







          // rectangle gate dragging and transformation

          function dragStart(event, d) {
            // box.raise().classed('active', true); // If uncommented, it would bring the rectangle to the front and add the class active.
            d.startX = event.x;
            d.startY = event.y;
            d.boxX = +box.attr('x');
            d.boxY = +box.attr('y');
          }

          function dragging(event, d) {
            if (gatesInfo.clip_gates === true) {
              // Calculate movement
              const dx = event.x - d.startX;
              const dy = event.y - d.startY;
          
              // Proposed new positions
              let newX = d.boxX + dx;
              let newY = d.boxY + dy;
          
              // Clamp new positions within plot boundaries
              newX = Math.max(0, Math.min(plotInfo.d3_x_res - +box.attr('width'), newX));
              newY = Math.max(0, Math.min(plotInfo.d3_y_res - +box.attr('height'), newY));
          
              // Update rectangle position
              box.attr('x', newX);
              box.attr('y', newY);
          
              // Update stored positions
              d.boxX = newX;
              d.boxY = newY;
              d.startX = event.x;
              d.startY = event.y;
          
              // Update handles' positions when dragging box
              handles.data([
                { x: newX, y: newY, cursor: 'nw-resize' },
                { x: newX + +box.attr('width'), y: newY, cursor: 'ne-resize' },
                { x: newX, y: newY + +box.attr('height'), cursor: 'sw-resize' },
                { x: newX + +box.attr('width'), y: newY + +box.attr('height'), cursor: 'se-resize' }
              ])
              .attr('cx', d => d.x)
              .attr('cy', d => d.y);

            } else {
              const newX = Math.max(0, Math.min(width - +box.attr('width'), event.x));
              const newY = Math.max(0, Math.min(height - +box.attr('height'), event.y));
              const dx = d.startX - event.x;
              const dy = d.startY - event.y;
  
              d.startX = event.x;
              d.startY = event.y;
              box.attr('x', d.boxX - dx);
              box.attr('y', d.boxY - dy);
              d.boxX = +box.attr('x');
              d.boxY = +box.attr('y');
  
              // Update handles' positions xhen dragging box
              handles.data([
                { x: d.boxX, y: d.boxY ,cursor: 'nw-resize'},
                { x: d.boxX + +box.attr('width'), y: d.boxY, cursor: 'ne-resize' },
                { x: d.boxX, y: d.boxY + +box.attr('height'), cursor: 'sw-resize' },
                { x: d.boxX + +box.attr('width'), y: d.boxY + +box.attr('height') , cursor: 'se-resize'}
              ])
              .attr('cx', d => d.x)
              .attr('cy', d => d.y);
            }
            
          }

          function dragEnd(event, d) {
            const rect = d3.select(this);
            sendGateUpdate(rect);
          }



          function handleDragStart(event, d) {
              
            d.startX = event.x;
            d.startY = event.y;
            d.initialBox = {
              x: +box.attr('x'),
              y: +box.attr('y'),
              width: +box.attr('width'),
              height: +box.attr('height')
            };

          }

          function handleDragging(event, d) {

            const dx = event.x - d.startX;
            const dy = event.y - d.startY;

            if (gatesInfo.clip_gates === true) {
                const dx = event.x - d.startX;
                const dy = event.y - d.startY;
            
                // Initialize new box properties with initial values
                let newX = d.initialBox.x;
                let newY = d.initialBox.y;
                let newWidth = d.initialBox.width;
                let newHeight = d.initialBox.height;
            
                if (d.cursor.includes('nw')) {
                  newX = d.initialBox.x + dx;
                  newY = d.initialBox.y + dy;
                  newWidth = d.initialBox.width - dx;
                  newHeight = d.initialBox.height - dy;
            
                  // Prevent dragging beyond borders
                  if (newX < 0) {
                    newWidth += newX;
                    newX = 0;
                  }
                  if (newY < 0) {
                    newHeight += newY;
                    newY = 0;
                  }
                  if (newWidth + newX > plotInfo.d3_x_res) {
                    newWidth = plotInfo.d3_x_res - newX;
                  }
                  if (newHeight + newY > plotInfo.d3_y_res) {
                    newHeight = plotInfo.d3_y_res - newY;
                  }
                  if (newWidth < 0) newWidth = 0;
                  if (newHeight < 0) newHeight = 0;
                } else if (d.cursor.includes('se')) {
                  newWidth = d.initialBox.width + dx;
                  newHeight = d.initialBox.height + dy;
            
                  // Prevent resizing beyond borders
                  if (d.initialBox.x + newWidth > plotInfo.d3_x_res) {
                    newWidth = plotInfo.d3_x_res - d.initialBox.x;
                  }
                  if (d.initialBox.y + newHeight > plotInfo.d3_y_res) {
                    newHeight = plotInfo.d3_y_res - d.initialBox.y;
                  }
                  if (newWidth < 0) newWidth = 0;
                  if (newHeight < 0) newHeight = 0;
                } else if (d.cursor.includes('sw')) {
                  newX = d.initialBox.x + dx;
                  newWidth = d.initialBox.width - dx;
                  newHeight = d.initialBox.height + dy;
            
                  if (newX < 0) {
                    newWidth += newX;
                    newX = 0;
                  }
                  if (d.initialBox.y + newHeight > plotInfo.d3_y_res) {
                    newHeight = plotInfo.d3_y_res - d.initialBox.y;
                  }
                  if (newWidth + newX > plotInfo.d3_x_res) {
                    newWidth = plotInfo.d3_x_res - newX;
                  }
                  if (newWidth < 0) newWidth = 0;
                  if (newHeight < 0) newHeight = 0;
                } else if (d.cursor.includes('ne')) {
                  newY = d.initialBox.y + dy;
                  newWidth = d.initialBox.width + dx;
                  newHeight = d.initialBox.height - dy;
            
                  if (newY < 0) {
                    newHeight += newY;
                    newY = 0;
                  }
                  if (d.initialBox.x + newWidth > plotInfo.d3_x_res) {
                    newWidth = plotInfo.d3_x_res - d.initialBox.x;
                  }
                  if (newHeight + newY > plotInfo.d3_y_res) {
                    newHeight = plotInfo.d3_y_res - newY;
                  }
                  if (newWidth < 0) newWidth = 0;
                  if (newHeight < 0) newHeight = 0;
                }
            
                // Update rectangle attributes
                box.attr('x', newX)
                  .attr('y', newY)
                  .attr('width', newWidth)
                  .attr('height', newHeight);
            
                // Update handles' positions during resizing
                handles.attr('cx', (d, i) => {
                  if (i == 0 || i == 2) return newX;
                  else return newX + newWidth;
                })
                .attr('cy', (d, i) => {
                  if (i == 0 || i == 1) return newY;
                  else return newY + newHeight;
                });
            
            } else {

              if (d.cursor.includes('nw')) {
                box.attr('x', Math.min(d.initialBox.x + d.initialBox.width, Math.max(0, d.initialBox.x + dx)));
                box.attr('y', Math.min(d.initialBox.y + d.initialBox.height, Math.max(0, d.initialBox.y + dy)));
                box.attr('width', Math.max(0, d.initialBox.width - dx));
                box.attr('height', Math.max(0, d.initialBox.height - dy));
              } else if  (d.cursor.includes('se')) {
                box.attr('width', Math.max(0, d.initialBox.width + dx));
                box.attr('height', Math.max(0, d.initialBox.height + dy));
              } else if  (d.cursor.includes('sw')) {
                box.attr('x', Math.min(d.initialBox.x + d.initialBox.width, Math.max(0, d.initialBox.x + dx)));
                box.attr('width', Math.max(0, d.initialBox.width - dx));
                box.attr('height', Math.max(0, d.initialBox.height + dy));
  
              } else if  (d.cursor.includes('ne')) {
                box.attr('y', Math.min(d.initialBox.y + d.initialBox.height, Math.max(0, d.initialBox.y + dy)));
                box.attr('width', Math.max(0, d.initialBox.width + dx));
                box.attr('height', Math.max(0, d.initialBox.height - dy));
              }
  
  
              // Update the position of all circles during resizing
              handles.attr('cx', (d, i) => {
                  let t = 0;
                if( i == 0){
                  t = +box.attr('x');
                } else if(i==1 ){
                  t = +box.attr('x') + +box.attr('width');
                } else if (i == 2){
                  t = +box.attr('x')
                } else if (i == 3){
                  t = +box.attr('x') + +box.attr('width');
                }
                return t;
              })
                .attr('cy', (d, i) => {
                let t = 0;
                if( i == 0)
                {
                  t = +box.attr('y');
                } else if( i == 1){
                  t = +box.attr('y');
                } else if( i ==2){
                  t = +box.attr('y') + +box.attr('height');
                }else if( i == 3){
                  t = +box.attr('y') + +box.attr('height');
                }
                return t;
              });
            }
          }

          function handleDragEnd(event, d) {
            const rect = d3.select(box.node());
            sendGateUpdate(rect);
          }

          function sendGateUpdate(rect) {
            const gateName = rect.attr('data-gate-name');
            const x = +rect.attr('x');
            const y = +rect.attr('y');
            const width = +rect.attr('width');
            const height = +rect.attr('height');
          
            // Convert screen coordinates back to data coordinates
            const x1 = plotInfo.xScale.invert(x);
            const x2 = plotInfo.xScale.invert(x + width);
            const y1 = plotInfo.yScale.invert(y + height); // Note the inversion of y
            const y2 = plotInfo.yScale.invert(y);
          
            const x_coords = [x1, x2].sort((a, b) => a - b);
            const y_coords = [y1, y2].sort((a, b) => a - b);
          
            // Send the updated coordinates back to R
            Shiny.setInputValue('gate_update', {
              gate_name: gateName,
              x_coords: x_coords,
              y_coords: y_coords,
              x_name: plotInfo.x_name,
              y_name: plotInfo.y_name
            }, { priority: 'event' });
          }

      }
    
      

    }
  }









}







// # JS code to handle plot clicks

Shiny.addCustomMessageHandler('scatter_click', function(click_coords) {
  console.log('Received coordinates from server:', click_coords);
  
  plotInfo.cx = ((click_coords[0] - plotInfo.x_axis_min) / plotInfo.x_total * plotInfo.d3_x_res);
  plotInfo.cy = plotInfo.d3_y_res - ((click_coords[1] - plotInfo.y_axis_min) / plotInfo.y_total * plotInfo.d3_y_res);
  console.log('Calculated point coordinates x:', plotInfo.cx);
  console.log('Calculated point coordinates y:', plotInfo.cy);  

  // Create SVG
  // plotInfo.gate_svg = d3.select('#d3_output_gates')
  //  .append('svg')
  //  .attr('width', plotInfo.d3_x_res)
  //  .attr('height', plotInfo.d3_y_res)
  //  .style('position', 'absolute')
  //  .style('z-index', '2000');

// Append a circle to the SVG
// plotInfo.gate_svg.append('circle')
//  .attr('cx', plotInfo.cx)
//  .attr('cy', plotInfo.cy)
//  .attr('r', 5)
//  .attr('fill', 'red');

//  console.log('Circle appended at:', plotInfo.cx, plotInfo.cy);
});



// # JS code to handle plot brush

Shiny.addCustomMessageHandler('detected_gates_biaxial', function(detected_gates_biaxial) {
  console.log('Received biaxial gate information from server');
  gatesInfo.detected_gates_biaxial = detected_gates_biaxial;
  redrawGatingSVG();
});


// # JS code to handle plot hover


// # JS code to handle plot double click to activate gate underneath





// # JS code for gating tree visualization

// Function to bind events to jsTree
function bindTreeEvents() {
  $('#gating_tree').on('select_node.jstree', function(e, data) {
    // Handle node selection
    var selectedNode = data.node;
    console.log('Node selected:', selectedNode.id);
    Shiny.setInputValue('selected_tree_node', selectedNode.id);
    updateGateHighlight(selectedNode.id);
    raiseSelectedGate(selectedNode.id);
  });

  $('#gating_tree').on('dblclick.jstree', function(e) {
    var instance = $.jstree.reference(this);
    var node = instance.get_node(e.target);
    console.log('Node double-clicked:', node.id);
    Shiny.setInputValue('activated_gate', node.id);
  });
}

// Handle the 'initTree' message from the server
Shiny.addCustomMessageHandler('initTree', function(message) {
  var treeData = message.data;

  // Unbind previous events
  $('#gating_tree').off('.jstree');

  // Destroy the tree if it exists
  if ($.jstree.reference('#gating_tree')) {
    $('#gating_tree').jstree('destroy');
  }

  // Initialize the tree
  $('#gating_tree').jstree({
      'core': {
          'data': treeData,
          'themes': {
              'icons': false  // Disable default folder icons
          },
          'multiple': false,  // Adjust as needed
          'dblclick_toggle': false    // Prevent nodes from toggling open/close on double-click
      },
      'plugins': ['state', 'contextmenu'],
      'state': { 'opened': true },  // Open all nodes by default
      'contextmenu': {
        'items': customMenu // Define custom menu function
      }
  });

  // Open all nodes after the tree is ready
  $('#gating_tree').on('ready.jstree', function() {
    $(this).jstree('open_all');
  });

  // Bind events
  bindTreeEvents();
});


// context menu for the jsTree
function customMenu(node) {
  var items = {
    activateItem: {
      label: "Activate",
      action: function () {
        console.log("Activate clicked on node:", node.id);
        Shiny.setInputValue('activated_gate', node.id);
      }
    },
    renameItem: {
      label: "Rename",
      action: function () {
        console.log("Rename clicked on node:", node.id);
        Shiny.setInputValue('rename_gate', node.id);
      }
    },
    deleteItem: {
      label: "Delete",
      submenu: {
        deleteSingle: {
          label: "Single",
          action: function () {
            console.log("Delete single clicked on node:", node.id);
            Shiny.setInputValue('delete_gate', node.id);
            Shiny.setInputValue('deletion_type', "single");
          }
        },
        deleteBranch: {
          label: "Branch",
          action: function () {
            console.log("Delete branch clicked on node:", node.id);
            Shiny.setInputValue('delete_gate', node.id);
            Shiny.setInputValue('deletion_type', "branch");
          }
        },
        deleteChildren: {
          label: "Children",
          action: function () {
            console.log("Delete children clicked on node:", node.id);
            Shiny.setInputValue('delete_gate', node.id);
            Shiny.setInputValue('deletion_type', "children");
          }
        }
      }
    },
    copyItem: {
      label: "Copy",
      submenu: {
        copySingle: {
          label: "Single",
          action: function () {
            console.log("Copy single clicked on node:", node.id);
            Shiny.setInputValue('copy_gate', node.id);
            Shiny.setInputValue('copy_type', "single");
          }
        },
        copyBranch: {
          label: "Branch",
          action: function () {
            console.log("Copy branch clicked on node:", node.id);
            Shiny.setInputValue('copy_gate', node.id);
            Shiny.setInputValue('copy_type', "branch");
          }
        },
        copyChildren: {
          label: "Children",
          action: function () {
            console.log("Copy children clicked on node:", node.id);
            Shiny.setInputValue('copy_gate', node.id);
            Shiny.setInputValue('copy_type', "children");
          }
        }
      }
    }
  };
  return items;
}



if (document.readyState === 'complete' || document.readyState === 'interactive') {
  $(document).ready(function() {
    initBotUI();
  });
} else {
  document.addEventListener('DOMContentLoaded', initBotUI);
}


function initBotUI() {
  if (document.getElementById('botui-app') && typeof BotUI !== 'undefined') {

    chatInfo.botui = new BotUI('botui-app');

    var loadingMsgIndex; // Variable to store loading message index

    // Start the conversation
    chatInfo.botui.message.add({
      content: 'Hello! How can I assist you today?'
    }).then(function () {
      startConversation();
    });

    // Function to start the conversation loop
    function startConversation() {
      chatInfo.botui.action.text({
        action: {
          placeholder: 'Type your message...'
        }
      }).then(function (res) {
        // Add user's message to the chat
        chatInfo.botui.message.add({
          human: true,
          content: res.value
        }).then(function () {
          // Show loading animation
          return chatInfo.botui.message.add({
            loading: true
          });
        }).then(function (index) {
          // Save the index of the loading message
          loadingMsgIndex = index;

          // Send user's input to Shiny
          Shiny.setInputValue('user_input', res.value);
        });
      });
    }

    // Handle responses from Shiny
    Shiny.addCustomMessageHandler('chatbot_response', function(strings) {
      chatInfo.response = strings.response;
      console.log(chatInfo.response);
      // Update the loading message with the bot's response
      chatInfo.botui.message.update(loadingMsgIndex, {
        content: strings.response,
        loading: false // Remove the loading animation
      }).then(function () {
        // Continue the conversation loop
        startConversation();
      });
    });

  } else {
    console.error('BotUI element not found or BotUI is not defined.');
  }
}

