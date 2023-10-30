// General-purpose interactive graph viewer, using D3.js

// Wait until the 'setup' function's depencencies are ready:
//  * The <script>-loaded d3.v7.min.js library
//  * The <script>-loaded 'data' variable containing JSON graph data
//  * The <body> DOM element
window.addEventListener('load',() => setup(window.data))

function setup(graph) {
  console.log('Data loaded from JS!')
  
  const colors = d3.scaleOrdinal(d3.schemeCategory10)
  const radius = 5

  // HTML element that nodes will be added under
  const root = d3.select("body")

  // Create SVG element
  const svg = root.append("svg").attr("id","chart")
    .attr("viewBox", [-500, -300, 1000, 600])

  // Add Mozilla's arrowhead marker to the SVG
  svg.append("defs").append("marker")
    .attr("id","arrow")
    .attr("viewBox", "0 0 10 10")
    .attr("refX", 30).attr("refY", 5)
    .attr("markerWidth",3).attr("markerHeight",6)
    .attr("orient","auto-start-reverse")
    .append("path")
    .attr("d","M 0 0 L 10 5 L 0 10 z")
    .attr("fill","context-stroke")

  // Make SVG zoomable & pannable
  const svg_contents = svg.append("g").attr("id","contents")
  svg.call(d3.zoom()
    .scaleExtent([1/2,4])
    .on("zoom",({transform}) => svg_contents.attr("transform", transform)))

  // Add edges to the SVG
  const svg_edges = svg_contents.append("g").attr("id","edges")
    .selectAll()
    .data(graph.edges)
    .join("line")

  // Add nodes to the SVG
  const svg_nodes = svg_contents.append("g").attr("id","nodes")
    .selectAll()
    .data(graph.nodes)
    .join("circle")
    .attr("r", radius)
    .attr("fill", d => colors(d.group))
  
  // Add node labels to SVG
  const svg_labels = svg_contents.append("g").attr("id","labels")
    .attr("id","labels")
    .selectAll()
    .data(graph.nodes)
    .join("text")
    .attr("class","label")
  svg_labels.append("tspan").html(d => (d.name || d.id) + "\n")
  
  // Create legend
  const legend = root.append("details").attr("id","legend")
  legend.append("summary").text("Legend")
  const legend_entries = legend.selectAll()
    .data([...new Set(graph.nodes.map(d => d.group))])
    .join("div")
  legend_entries
    .append("svg")
    .attr("viewBox", [-1,-1,2,2])
    .attr("height","1em")
    .append("circle")
    .attr("r",1)
    .attr("fill", g => colors(g))
  legend_entries
    .append("span")
    .text(g => g)

  // Misc. helpers
  const labels_of = n => svg_labels.filter(l => l.id == n.id)
  const legends_of = n => legend_entries.filter(g => g == n.group)
  const nodes_of = g => svg_nodes.filter(n => n.group == g)
  const set_class = (cls,sel,v) => sel.classed(cls, _ => v)
  const toggle_class = (cls,sel) => set_class(cls,sel,!sel.classed(cls))

  // When hovering over a node, peek the label and highlight the legend
  svg_nodes
    .on("mouseover", e => d3
      .select(e.currentTarget)
      .each(n => set_class("peeking",labels_of(n), true))
      .each(n => set_class("highlighted",legends_of(n), true))
      .call(sel => set_class("highlighted",sel,true))
      )
    .on("mouseout", e => d3
      .select(e.currentTarget)
      .each(n => set_class("peeking",labels_of(n), false))
      .each(n => set_class("highlighted",legends_of(n), false))
      .call(sel => set_class("highlighted",sel,false))
      )
    // When a node is clicked, pin/unpin its label
    .on("click", e => d3
      .select(e.currentTarget)
      .each(n => toggle_class("pinned",labels_of(n)))
      )

  // When hovering over a legend entry, highlight all its nodes
  legend_entries
    .on("mouseover", e => d3
        .select(e.currentTarget)
        .each(n => set_class("highlighted",nodes_of(n), true))
        .call(sel => set_class("highlighted",sel,true))
        )
    .on("mouseout", e => d3
        .select(e.currentTarget)
        .each(n => set_class("highlighted",nodes_of(n), false))
        .call(sel => set_class("highlighted",sel,false))
        )

  // Create a simulation with several forces.
  const simulation = d3
    .forceSimulation(graph.nodes)
    .force("edges attract", d3.forceLink(graph.edges).id(d => d.id))
    .force("nodes repel", d3.forceManyBody())
    .force("left/right repel", d3.forceX())
    .force("top/bottom repel", d3.forceY())
    // Every simuation tick, update SVG element positions
    .on("tick", _ => {
      svg_edges
        .attr("x1", d => d.source.x)
        .attr("y1", d => d.source.y)
        .attr("x2", d => d.target.x)
        .attr("y2", d => d.target.y)
      svg_nodes
        .attr("cx", d => d.x)
        .attr("cy", d => d.y)
      svg_labels
        .attr("x", d => d.x)
        .attr("y", d => d.y)
    })

  // Allow nodes to be dragged about with the mouse
  svg_nodes.call(d3.drag()
    // When drag starts, start reheating simulation (to 1/3 of initial heat)
    .on("start", event => simulation.alphaTarget(0.3).restart())
    // Durring dragging, force the dragged node's position
    .on("drag", event => {
      event.subject.fx = event.x
      event.subject.fy = event.y
    })
    // After dragging, allow the dragged node to move again & cool down simulation
    .on("end", event => {
      simulation.alphaTarget(0)
      event.subject.fx = null
      event.subject.fy = null
    }))

  console.log('setup complete, graph should now be visible')
}

// Credits:
//  Using d3.js for definition dependency visualization: https://1lab.dev/
//  Setting up d3.js: https://observablehq.com/@d3/force-directed-graph/2

