// Create resizable and draggable box with handles
const box = svg.append("rect")
.attr("x", 100)
.attr("y", 100)
.attr("width", 100)
.attr("height", 100)
.attr("class", "box")
.style("cursor", "pointer")
.call(d3.drag()
  .on("start", dragStart)
  .on("drag", dragging)      
);
box.data([
{startX: +box.attr("x")},
{startY: +box.attr("y")}
]);


function dragStart(event, d) {

// box.raise().classed("active", true);
d.startX = event.x;
d.startY = event.y;
d.boxX = +box.attr("x");
d.boxY = +box.attr("y");
}

function dragging(event, d) {

const newX = Math.max(0, Math.min(width - +box.attr("width"), event.x));
const newY = Math.max(0, Math.min(height - +box.attr("height"), event.y));
const dx = d.startX - event.x;
const dy = d.startY - event.y;

d.startX = event.x;
d.startY = event.y;
box.attr("x", d.boxX - dx);
box.attr("y", d.boxY - dy);
d.boxX = +box.attr("x");
d.boxY = +box.attr("y");

// Update handles' positions xhen dragging box
handles.data([
  { x: d.boxX, y: d.boxY ,cursor: "nw-resize"},
  { x: d.boxX + +box.attr("width"), y: d.boxY, cursor: "ne-resize" },
  { x: d.boxX, y: d.boxY + +box.attr("height"), cursor: "sw-resize" },
  { x: d.boxX + +box.attr("width"), y: d.boxY + +box.attr("height") , cursor: "se-resize"}
])
.attr("cx", d => d.x)
.attr("cy", d => d.y);
}

//function dragEnd(event) {
//  box.classed("active", false);
//}

const handles = svg.selectAll("circle")
.data([
  { x: 100, y: 100, cursor: "nw-resize" },
  { x: 200, y: 100, cursor: "ne-resize" },
  { x: 100, y: 200, cursor: "sw-resize" },
  { x: 200, y: 200, cursor: "se-resize" }
])
.enter().append("circle")
.attr("class", "handle")
.attr("cx", d => d.x)
.attr("cy", d => d.y)
.attr("r", handleRadius)
.style("cursor", d => d.cursor)
.call(d3.drag()
  .on("start", handleDragStart)
  .on("drag", handleDragging)
);


let rotating = false;

d3.select("body")
.on("keydown", (event) => {
  if (event.key === "Shift") {
    rotating = true;
  }
})
.on("keyup", (event) => {
  if (event.key === "Shift") {
    rotating = false;
  }
});

function handleDragStart(event, d) {
    
d.startX = event.x;
d.startY = event.y;
d.initialBox = {
  x: +box.attr("x"),
  y: +box.attr("y"),
  width: +box.attr("width"),
  height: +box.attr("height"),
  rotation: +box.attr("transform")?.split("(")[1].split(")")[0] || 0
};

}

function handleDragging(event, d) {

const dx = event.x - d.startX;
const dy = event.y - d.startY;


if (d.cursor.includes("nw")) {
  box.attr("x", Math.min(d.initialBox.x + d.initialBox.width, Math.max(0, d.initialBox.x + dx)));
  box.attr("y", Math.min(d.initialBox.y + d.initialBox.height, Math.max(0, d.initialBox.y + dy)));
  box.attr("width", Math.max(0, d.initialBox.width - dx));
  box.attr("height", Math.max(0, d.initialBox.height - dy));
} else if  (d.cursor.includes("se")) {
  box.attr("width", Math.max(0, d.initialBox.width + dx));
  box.attr("height", Math.max(0, d.initialBox.height + dy));
} else if  (d.cursor.includes("sw")) {
    box.attr("x", Math.min(d.initialBox.x + d.initialBox.width, Math.max(0, d.initialBox.x + dx)));
  box.attr("width", Math.max(0, d.initialBox.width - dx));
  box.attr("height", Math.max(0, d.initialBox.height + dy));

} else if  (d.cursor.includes("ne")) {
    box.attr("y", Math.min(d.initialBox.y + d.initialBox.height, Math.max(0, d.initialBox.y + dy)));
  box.attr("width", Math.max(0, d.initialBox.width + dx));
  box.attr("height", Math.max(0, d.initialBox.height - dy));
}


// Update the position of all circles during resizing
handles.attr("cx", (d, i) => {
    let t = 0;
  if( i == 0){
    t = +box.attr("x");
  } else if(i==1 ){
    t = +box.attr("x") + +box.attr("width");
  } else if (i == 2){
    t = +box.attr("x")
  } else if (i == 3){
    t = +box.attr("x") + +box.attr("width");
  }
  return t;
})
  .attr("cy", (d, i) => {
  let t = 0;
  if( i == 0)
  {
    t = +box.attr("y");
  } else if( i == 1){
    t = +box.attr("y");
  } else if( i ==2){
    t = +box.attr("y") + +box.attr("height");
  }else if( i == 3){
    t = +box.attr("y") + +box.attr("height");
  }
  return t;
 });
}