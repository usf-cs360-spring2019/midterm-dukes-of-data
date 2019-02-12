var max
d3.csv("police.csv", function(row){
  //console.log(row);
return row;
}).then(function(data2){
  console.log(data2.length);
  //for(var i =0; i <data2.length; i++){
console.log(data2[0]["Incident Date"]);
//return data2;
//SET MAX

//console.log(row);



  // get the data to visualize
//  let count = updateData()
//console.log(count);
  // get the svg to draw on
  let svg = d3.select("body").select("svg");

  let count = d3.map();

  /*
   * you can loop through strings as if they are arrays
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for
   */
   var category
  for (let i = 0; i < data2.length; i++) {
  category = data2[i]["Incident Category"];
    //console.log(category);
    if (count.has(category)) {
      count.set(category, count.get(category) + 1);
    }
    else {
      count.set(category, 1);
    }

    // check if we have seen this letter before

  }


  /*
   * we will need to map our data domain to our svg range, which
   * means we need to calculate the min and max of our data
   */

  let countMin = 0; // always include 0 in a bar chart!
  let countMax = d3.max(count.values());

  // this catches the case where all the bars are removed, so there
  // is no maximum value to compute
  //if (isNaN(countMax)) {
    //countMax = 0;
  //}

  // console.log("count bounds:", [countMin, countMax]);

  /*
   * before we draw, we should decide what kind of margins we
   * want. this will be the space around the core plot area,
   * where the tick marks and axis labels will be placed
   * http://bl.ocks.org/mbostock/3019563
   */
  let margin = {
    top:    15,
    right:  50, // leave space for y-axis
    bottom: 180, // leave space for x-axis
    left:   10
  };

  // now we can calculate how much space we have to plot
  let bounds = svg.node().getBoundingClientRect();
  let plotWidth = bounds.width - margin.right - margin.left;
  let plotHeight = bounds.height - margin.top - margin.bottom;

  /*
   * okay now somehow we have to figure out how to map a count value
   * to a bar height, decide bar widths, and figure out how to space
   * bars for each letter along the x-axis
   *
   * this is where the scales in d3 come in very handy
   * https://github.com/d3/d3-scale#api-reference
   */

  /*
   * the counts are easiest because they are numbers and we can use
   * a simple linear scale, but the complicating matter is the
   * coordinate system in svgs:
   * https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Positions
   *
   * so we want to map our min count (0) to the max height of the plot area
   */
  let countScale = d3.scaleLinear()
    .domain([countMin, countMax])
    .range([plotHeight, 0])
    .nice(); // rounds the domain a bit for nicer output

  /*
   * the letters need an ordinal scale instead, which is used for
   * categorical data. we want a bar space for all letters, not just
   * the ones we found, and spaces between bars.
   * https://github.com/d3/d3-scale#band-scales
   */
   //var cat= "category" + "gory"
   var cat = data2[8]["Incident Category"]
let letterScale
  //for(var l=2; l<data2.length;l++){
   letterScale = d3.scaleBand()
     //.domain([data2[l]["Incident Category"], data2[l-1]["Incident Category"],  data2[l-2]["Incident Category"],data2[l]["Incident Category"]]) // all letters (not using the count here)
     .domain(["Assault", "Burglary", "Arson","Case Closure","Civil Sidewalks","Courtesy Report", "Disorderly Conduct","Drug Offense","Drug Violantion","Embezzlement", "Family Offense", "Fire Report","Forgery And Counterfeiting", "Fraud","Gambling","Homocide","Juvenile Offenses", "Larceny Theft","Liquor Laws", "Lost Property","Malicious Mischief","Miscellaneous Investigation","Missing Person","Motor Vehicle Theft?","Motor Vehicle Theft","Non-Criminal", "Offences Against The Family And Children", "Other","Other Miscellaneous","Other Offenses","Prostitution", "Rape", "Recovered Vehicle", "Robbery","Sex Offense", "Stolen Property","Suicide", "Suspecious", "Suspicious Occ","Traffic Collision", "Traffic Violation Arrest", "Vandalism","Vehicle Impounded","Vehicle Misplaced","Warrant","Weapons Carrying Etc", "Weapons Offense", "Motor Vehicle Theft?", "Rape","Drug Violation", "Suicide","Homocide", "Suspicious", "Vehicle Impounded"])
    .rangeRound([0, plotWidth])
    .paddingInner(0.1); // space between bars
//console.log(data2[l]["Incident Category"])
//}
  // try using these scales in the console
  // console.log("count scale [0, 36]:", [countScale(0), countScale(36)]);


  // console.log("letter scale [a, z]:", [letterScale('a'), letterScale('z')]);

  // we are actually going to draw on the "plot area"
  let plot = svg.select("g#plot");

  if (plot.size() < 1) {
    // this is the first time we called this function
    // we need to steup the plot area
    plot = svg.append("g").attr("id", "plot");

    // notice in the "elements" view we now have a g element!

    // shift the plot area over by our margins to leave room
    // for the x- and y-axis
    plot.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  }

  // now lets draw our x- and y-axis
  // these require our x (letter) and y (count) scales
  let xAxis = d3.axisBottom(letterScale);
  let yAxis = d3.axisRight(countScale);

  // check if we have already drawn our axes
  if (plot.select("g#y-axis").size() < 1) {
    let xGroup = plot.append("g").attr("id", "x-axis");

    // the drawing is triggered by call()
    xGroup.call(xAxis);

    // notice it is at the top of our svg
    // we need to translate/shift it down to the bottom
    xGroup.attr("transform",  "translate(0," + plotHeight + ")");
    xGroup.selectAll("text")
    .attr("y", 0)
    .attr("x", 9)
    .attr("dy", ".35em")
    .attr("transform", "rotate(90)")
    .style("text-anchor", "start");
    //xGroup.attr("transform", "rotate(90)")
    // do the same for our y axix
    let yGroup = plot.append("g").attr("id", "y-axis");
    yGroup.call(yAxis);
    yGroup.attr("transform", "translate(" + plotWidth + ",0)");
  }
  else {
    // we need to do this so our chart updates
    // as we type new letters in our box
    plot.select("g#y-axis").call(yAxis);
  }

  // now how about some bars!
  /*
   * time to bind each data element to a rectangle in our visualization
   * hence the name data-driven documents (d3)
   */
  let bars = plot.selectAll("rect")
    .data(count.entries(), function(d) { return d.key; });

  // setting the "key" is important... this is how d3 will tell
  // what is existing data, new data, or old data

  /*
   * okay, this is where things get weird. d3 uses an enter, update,
   * exit pattern for dealing with data. think of it as new data,
   * existing data, and old data. for the first time, everything is new!
   * http://bost.ocks.org/mike/join/
   */
   console.log(category);
  // we use the enter() selection to add new bars for new data
  bars.enter().append("rect")
    // we will style using css
    .attr("class", "bar")
    // the width of our bar is determined by our band scale
    .attr("width", letterScale.bandwidth())
    // we must now map our letter to an x pixel position
//////////////////////////////////////
    .attr("fill", function(d) {

    if (d.key==="Assault" || d.key=== "Arson" || d.key==="Burglary"|| d.key==="Family Offense" || d.key==="Homocide" || d.key==="Rape" || d.key==="Robbery" || d.key==="Sex Offense" || d.key==="Warrant" || d.key==="Weapons Offense" ) { //Red
      return "red";
    } else if(d.key==="Non-Criminal"){
      return "blue"
    }else if(d.key==="Case Closure" || d.key==="Civil Sidewalks" || d.key==="Courtesy Report" || d.key==="Embezzlement" || d.key==="Fraud" || d.key==="Liquor Laws" || d.key==="Lost Property" || d.key==="Miscellaneous Investigation" || d.key==="Missing Person" || d.key==="Other" || d.key==="Other Miscellaneous" || d.key==="Other Offenses" || d.key==="Recovered Vehicle" || d.key==="Suicide" || d.key==="Traffic Collision" || d.key==="Traffic Violation Arrest" || d.key==="Vehicle Impounded") {  //green
      return "green";
    } else if (d.key === "Drug Offenses" || d.key==="Disordely Conduct" || d.key==="Drug Violation" || "Fire Report" || "Forgery And Counterfeiting" || d.key==="Larceny Theft" || d.key==="Malicious Mischief" || d.key==="Motor Vehicle Theft" || d.key==="Motor Vehicle Theft?" || d.key==="Offenses Against The Family And Children" || d.key==="Prostitution" || d.key==="Stolen Property" || d.key==="Suspecious" || d.key==="Suspecious Occ" || d.key==="Vandalism"|| d.key==="Weapons Carrying Etc") { //Orange
      return "orange";

    }
    return "pink";
  })
  //////////////////////////////////////
    .attr("x", function(d) {
      return letterScale(d.key);
    })
    // and do something similar for our y pixel position
    .attr("y", function(d) {
      return countScale(d.value);
    })
    // here it gets weird again, how do we set the bar height?
    .attr("height", function(d) {
      return plotHeight - countScale(d.value);
    })
    .each(function(d, i, nodes) {
      console.log("Added bar for:", d.key);
    });

  // notice there will not be bars created for missing letters!

  // so what happens when we change the text?
  // well our data changed, and there will be a new enter selection!
  // only new letters will get new bars

  // but we have to bind this draw function to textarea events
  // (see index.html)

  // for bars that already existed, we must use the update selection
  // and then update their height accordingly
  // we use transitions for this to avoid change blindness
  bars.transition()
    .attr("y", function(d) { return countScale(d.value); })
    .attr("height", function(d) { return plotHeight - countScale(d.value); });

  // what about letters that disappeared?
  // we use the exit selection for those to remove the bars
  bars.exit()
    .each(function(d, i, nodes) {
      console.log("Removing bar for:", d.key);
    })

    .attr("y", function(d) { return countScale(countMin); })
    .attr("height", function(d) { return plotHeight - countScale(countMin); })
    .remove();

})
