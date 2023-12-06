// General-purpose interactive graph viewer, using D3.js

// Wait until the 'setup' function's depencencies are ready:
//  * The <script>-loaded d3.v7.min.js library
//  * The <script>-loaded 'data' variable containing JSON graph data
//  * The <body> DOM element
window.addEventListener('load',() => setup(window.data))

function setup(graph) {
  console.log('Data loaded from JS!')
  
  const colors = d3.scaleOrdinal(d3.schemeCategory10)
  const min_radius = 5
  // Constants for simulation
  const link_rest_length = 5  // Too low => unstable, too high => no clustering
  const charge_per_mass = 80

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
    .join("a")
    .attr("href", n => n.hyperlink)


  // Helper functions
  const group_amount = group => group.amount // getter
  const node_mass = node => d3.sum(node.groups.map(group_amount))
  const node_radius = node => min_radius * Math.sqrt(node_mass(node))

  // Draw each node as a pie chart (by group breakdown)
  // (when each node has exactly one group, this just draws circles)
  const svg_segments = svg_nodes
    .selectAll()
    .data(node =>
      d3.pie()
        .value(group_amount)(node.groups)
        .map(seg_data => ({seg_data: seg_data, node: node}))
      )
    .join("path")
    .attr("fill", segment => colors(segment.seg_data.data.gname))
    .attr("d", segment => d3.arc()({
      innerRadius: 0,
      outerRadius: node_radius(segment.node),
      startAngle: segment.seg_data.startAngle,
      endAngle: segment.seg_data.endAngle
    }))
  
  // Add node labels to SVG
  const svg_labels = svg_contents.append("g").attr("id","labels")
    .attr("id","labels")
    .selectAll()
    .data(graph.nodes)
    .join("text")
    .attr("class","label")
  svg_labels.append("tspan").html(n => (n.name || n.id) + "\n")
  
  // De-duplicated list of all group names that appear in the dataset
  const groups = [...new Set([].concat(...new Set(graph.nodes.map(n => n.groups.map(g => g.gname)))))]

  // Create legend
  const legend = root.append("details").attr("id","legend")
  legend.append("summary").text("Legend")
  const legend_entries = legend.selectAll()
    .data(groups)
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
  const legends_of = segment => legend_entries.filter(gname => gname == segment.seg_data.data.gname)
  const segments_of = gname => svg_segments.filter(segment => segment.seg_data.data.gname == gname)
  const set_class = (cls,sel,v) => sel.classed(cls, _ => v)
  const toggle_class = (cls,sel) => set_class(cls,sel,!sel.classed(cls))

  // When hovering over a node, peek the label
  svg_nodes
    .on("mouseover", e => d3
      .select(e.currentTarget)
      .each(n => set_class("peeking",labels_of(n), true))
      )
    .on("mouseout", e => d3
      .select(e.currentTarget)
      .each(n => set_class("peeking",labels_of(n), false))
      )
    // When a node is clicked, pin/unpin its label
    .on("click", e => d3
      .select(e.currentTarget)
      .each(n => toggle_class("pinned",labels_of(n)))
      )
  
  // When hovering over a segment, highlight its legend entry
  svg_segments
    .on("mouseover", e => d3
      .select(e.currentTarget)
      .each(n => {
        set_class("highlighted",legends_of(n), true)
        legends_of(n).node().scrollIntoView(true)
        })
      .call(sel => set_class("highlighted",sel,true))
      )
    .on("mouseout", e => d3
      .select(e.currentTarget)
      .each(n => set_class("highlighted",legends_of(n), false))
      .call(sel => set_class("highlighted",sel,false))
      )

  // When hovering over a legend entry, highlight all its segments
  legend_entries
    .on("mouseover", e => d3
        .select(e.currentTarget)
        .each(n => set_class("highlighted",segments_of(n), true))
        .call(sel => set_class("highlighted",sel,true))
        )
    .on("mouseout", e => d3
        .select(e.currentTarget)
        .each(n => set_class("highlighted",segments_of(n), false))
        .call(sel => set_class("highlighted",sel,false))
        )

  // Create a simulation with several forces.
  const simulation = d3
    .forceSimulation(graph.nodes)
    .force("edges attract", d3.forceLink(graph.edges).id(node => node.id)
      // Adjust rest length of links to account for variable node radii
      .distance(edge => link_rest_length + node_radius(edge.source) + node_radius(edge.target)))
    .force("nodes repel", d3.forceManyBody()
      // Big nodes repel more
      .strength(n => -charge_per_mass * node_mass(n)))
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
        .attr("transform",d=>"translate("+d.x+","+d.y+")")
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
//  d3.js force layout: https://observablehq.com/@d3/force-directed-graph/2
//  d3.js sized pies: https://observablehq.com/@analyzer2004/bubble-pie-chart
