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
      
        const box = gatesInfo.gating_svg.append('rect')
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
          );
      
        box.data([{startX: +box.attr('x'), startY: +box.attr('y')}]);
      
        const handles = gatesInfo.gating_svg.selectAll(`circle.handle-${i}`)
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
          );
      
        // Rectangle gate dragging and transformation
      
        function dragStart(event, d) {
          d.startX = event.x;
          d.startY = event.y;
          d.boxX = +box.attr('x');
          d.boxY = +box.attr('y');
        }
      
        function dragging(event, d) {
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
        }
      }