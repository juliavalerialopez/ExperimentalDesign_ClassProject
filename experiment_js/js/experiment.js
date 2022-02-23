var touchstone = 1;

var state = {
  NONE: 0,
  INSTRUCTIONS: 1,
  SHAPES: 2,
  PLACEHOLDERS: 3,
};

var ctx = {
  w: 800,
  h: 600,

  trials: [],
  participant: "",
  startBlock: 0,
  startTrial: 0,
  cpt: 0,

  participantIndex: touchstone == 1 ? "Participant" : "ParticipantID",
  practiceIndex: "Practice",
  blockIndex: (touchstone == 1 ? "Block" : "Block1"),
  trialIndex: (touchstone == 1 ? "Trial" : "TrialID"),
  vvIndex: "VV",
  objectsCountIndex: "OC",

  state: state.NONE,
  targetIndex: 0,

  startTime : new Date(),
  searchTime: 0,
  errorCount:0,
  // TODO log measures
  // loggedTrials is a 2-dimensional array where we store our log file
  // where one line is one trial
  loggedTrials:
    touchstone == 1 ?
      [["Participant", "Practice", "Block", "Trial", "VV", "OC", "visualSearchTime", "ErrorCount"]] :
      [["DesignName", "ParticipantID", "TrialID", "Block1", "Trial", "VV", "OC", "visualSearchTime", "ErrorCount"]]
};

/****************************************/
/********** LOAD CSV DESIGN FILE ********/
/****************************************/

var loadData = function (svgEl) {
  // d3.csv parses a csv file...
  d3.csv("experiment_touchstone" + touchstone + ".csv").then(function (data) {
    // ... and turns it into a 2-dimensional array where each line is an array indexed by the column headers
    // for example, data[2]["OC"] returns the value of OC in the 3rd line
    ctx.trials = data;
    // all trials for the whole experiment are stored in global variable ctx.trials

    var participant = "";
    var options = [];

    for (var i = 0; i < ctx.trials.length; i++) {
      ctx.trials[i].errorCount = 0;
      if (!(ctx.trials[i][ctx.participantIndex] === participant)) {
        participant = ctx.trials[i][ctx.participantIndex];
        options.push(participant);
      }
    }

    var select = d3.select("#participantSel")
    select.selectAll("option")
      .data(options)
      .enter()
      .append("option")
      .text(function (d) { return d; });

    setParticipant(options[0]);


  }).catch(function (error) { console.log(error) });
};

/****************************************/
/************* RUN EXPERIMENT ***********/
/****************************************/


var startExperiment = function (event) {
  event.preventDefault();

  // set the trial counter to the first trial to run
  // ctx.participant, ctx.startBlock and ctx.startTrial contain values selected in combo boxes

  for (var i = 0; i < ctx.trials.length; i++) {

    if (ctx.trials[i][ctx.participantIndex] === ctx.participant) {
      if (parseInt(ctx.trials[i][ctx.blockIndex]) == ctx.startBlock
        && (touchstone == 2 || ctx.trials[i][ctx.practiceIndex].toLowerCase() === "false")) {
        if (parseInt(ctx.trials[i][ctx.trialIndex]) == ctx.startTrial) {
          ctx.cpt = i - 1;

          if (touchstone == 1) { // include practice trials before this trial for TouchStone 1
            while (ctx.cpt >= 0 && ctx.trials[ctx.cpt][ctx.practiceIndex].toLowerCase() === "true") {
              ctx.cpt = ctx.cpt - 1;
            }
          }

          // start first trial
          //console.log("start experiment at " + ctx.cpt);
          nextTrial();
          return;
        }
      }
    }
  }

}

var nextTrial = function () {
  ctx.cpt++;
  if (ctx.trials[ctx.cpt][ctx.participantIndex] !== ctx.participant) return;
    displayInstructions();
}

var displayInstructions = function () {
  ctx.state = state.INSTRUCTIONS;

  d3.select("#instr")
    .append("div")
    .attr("id", "instructions")
    .classed("instr", true);

  d3.select("#instructions")
    .append("p")
    .html("Multiple shapes will get displayed.<br> Only <b>one shape</b> is different from all other shapes.");

  d3.select("#instructions")
    .append("p")
    .html("1. Spot it as fast as possible and press <code>Space</code> bar;");

  d3.select("#instructions")
    .append("p")
    .html("2. Click on the placeholder over that shape.");

  d3.select("#instructions")
    .append("p")
    .html("Press <code>Enter</code> key when ready to start.");

}

var displayShapes = function () {
  ctx.state = state.SHAPES;

  var visualVariable = ctx.trials[ctx.cpt]["VV"];
  var oc = ctx.trials[ctx.cpt]["OC"];
  if (oc === "Small") { //Small  Low
    objectCount = 9;
  } else if (oc === "Medium") {
    objectCount = 25;
  } else {
    objectCount = 49;
  }

  var svgElement = d3.select("svg");
  var group = svgElement.append("g")
    .attr("id", "shapes")
    .attr("transform", "translate(100,100)");

  // 1. Decide on the visual appearance of the target
  // In my example, it means deciding on its size (large or small) and its color (light or dark)
  var randomNumber1 = Math.random();
  var randomNumber2 = Math.random();
  var targetShape, targetBorder;
  if (randomNumber1 > 0.5) {
    targetShape = "rect"; // target is large  "circle"
  } else {
    targetShape = "circle"; // target is small "square"
  }
  if (randomNumber2 > 0.5) {
    targetBorder = "0%"; // target is blue
  } else {
    targetBorder = "1%"; // target is orange
  }

  // 2. Set the visual appearance of all other objects now that the target appearance is decided
  // Here, we implement the case VV = "Size" so all other objects are large (resp. small) if target is small (resp. large) but have the same color as target.
  var objectsAppearance = [];
  //VV1
  if (visualVariable == "VV1") { //Size VV1
    for (var i = 0; i < objectCount - 1; i++) {
      if (targetShape == "rect") {
        objectsAppearance.push({
          shape: "circle",
          strokeWidth: targetBorder
        });
      } else {
        objectsAppearance.push({
          shape: "rect",
          strokeWidth: targetBorder
        });
      }
    }
  }
  // VV2
  if (visualVariable == "VV2") { //Color VV2
    for (var i = 0; i < objectCount - 1; i++) {
      if (targetBorder == "1%") {
        objectsAppearance.push({
          shape: targetShape,
          strokeWidth: "0%"
        });
      } else {
        objectsAppearance.push({
          shape: targetShape,
          strokeWidth: "1%"
        });
      }
    }
  }

  // VV1VV2
  if (visualVariable == "VV1VV2") { //ColorSize VV1VV2

    //console.log("Target: " + targetBorder + ", " + targetShape);
    var smallerSize = parseInt((objectCount - 1) / 3);
    //Fill 1/3 of objects with the same size but the opposite color
    for (var i = 0; i < smallerSize; i++) {
      if (targetBorder == "1%") {
        objectsAppearance.push({
          shape: targetShape,
          strokeWidth: "0%"
        });
      } else {
        objectsAppearance.push({
          shape: targetShape,
          strokeWidth: "1%"
        });
      }
    }
    //Fill 1/3 of objects with the same color but the opposite size
    for (var i = smallerSize; i < (smallerSize * 2) - 1; i++) {
      if (targetShape == "rect") {
        objectsAppearance.push({
          shape: "circle",
          strokeWidth: targetBorder
        });
      } else {
        objectsAppearance.push({
          shape: "rect",
          strokeWidth: targetBorder
        });
      }
    }

    //Fill the rest of the objectrecs with the oposite color and size
    for (var i = objectsAppearance.length ; i < objectCount-1; i++) {
      //if (targetBorder == "Orange") {
        objectsAppearance.push({
          shape: (targetShape == "rect" ? "circle" : "rect"),
          strokeWidth: (targetBorder == "0%"? "1": "0%")
        });
    }
  }

  

  // 3. Shuffle the list of objects (useful when there are variations regarding both visual variable) and add the target at a specific index
  shuffle(objectsAppearance);
  // draw a random index for the target
  ctx.targetIndex = Math.floor(Math.random() * objectCount);
  // and insert it at this specific index
  objectsAppearance.splice(ctx.targetIndex, 0, { shape: targetShape, strokeWidth: targetBorder });

  // 4. We create actual SVG shapes and lay them out as a grid
  // compute coordinates for laying out objects as a grid
  var gridCoords = gridCoordinates(objectCount, 60);
  // display all objects by adding actual SVG shapes
  for (var i = 0; i < objectCount; i++) {
    if( objectsAppearance[i].shape == "circle"){
    group.append("circle")
      .attr("cx", gridCoords[i].x)
      .attr("cy", gridCoords[i].y)
      .attr("r", 20)
      .attr("stroke","#D794D0")
      .attr("stroke-width", objectsAppearance[i].strokeWidth )
      //.attr("stroke", "#D794D0")
      //.attr( "stroke")
      .attr("fill", '#1371FF');
    } else {
      group.append("rect")
      .attr("x", gridCoords[i].x - 20)
      .attr("y", gridCoords[i].y - 20)
      .attr("width", 40)
      .attr("height", 40)
      .attr("stroke","#D794D0")
      .attr("stroke-width", objectsAppearance[i].strokeWidth )
      //.attr( "stroke")
      //.attr("stroke", "#D794D0")
      .attr("fill", '#1371FF');

    }
  }
  ctx.startTime = new Date();
}

var displayPlaceholders = function () {
  ctx.state = state.PLACEHOLDERS;

  var oc = ctx.trials[ctx.cpt]["OC"];
  var objectCount = 0;

  if (oc === "Small") { // Small Low
    objectCount = 9;
  } else if (oc === "Medium") {
    objectCount = 25;
  } else {
    objectCount = 49;
  }

  var svgElement = d3.select("svg");
  var group = svgElement.append("g")
    .attr("id", "placeholders")
    .attr("transform", "translate(100,100)");

  var gridCoords = gridCoordinates(objectCount, 60);
  for (var i = 0; i < objectCount; i++) {
    var placeholder = group.append("rect")
      .attr("x", gridCoords[i].x - 28)
      .attr("y", gridCoords[i].y - 28)
      .attr("width", 56)
      .attr("height", 56)
      .attr("fill", "Gray");

      let currentIndex = i;

    placeholder.on("click",
      function () {
        console.log(ctx.loggedTrials.length);
         //["DesignName","ParticipantID","TrialID","Block","Trial","VV","OC","visualSearchTime","ErrorCount"]];
         if(ctx.targetIndex === currentIndex ){//|| ctx.trials[ctx.cpt][ctx.practiceIndex]) {
          ctx.loggedTrials.push([
            ctx.trials[ctx.cpt][ctx.participantIndex],//Participant
            ctx.trials[ctx.cpt][ctx.practiceIndex], //TrialID
            ctx.trials[ctx.cpt][ctx.blockIndex], //Block
            ctx.trials[ctx.cpt][ctx.trialIndex], //Trial
            ctx.trials[ctx.cpt][ctx.vvIndex], //VV
            oc, //OC
            ctx.trials[ctx.cpt].searchTime , //VisualVariable
            ctx.trials[ctx.cpt].errorCount //ErrorCount
            ]);
          } else {
            ctx.trials[ctx.cpt].errorCount++;
            console.log ( "error count " + ctx.trials[ctx.cpt].errorCount);

            console.log("Error: Blcok at " +ctx.trials[ctx.cpt][ctx.blockIndex] + 
            ", Trial:"+ ctx.trials[ctx.cpt][ctx.trialIndex]);
            var count = 0;
            for (var i = 0; i < ctx.trials.length; i++) {
              if (ctx.trials[i][ctx.participantIndex] == ctx.participant) {
              count++;
              }
            }
            var statrPos = ctx.participant * 50;
            var endPos = statrPos + count;
            var currentPos = ctx.cpt;//ctx.loggedTrials.length -1;
            console.log( "Started the amount at:" + statrPos +
            " and end at "+ endPos+ " currently at " + currentPos);
            var randomNewPos = Math.floor((Math.random() * (endPos - currentPos)) + currentPos);
            console.log ( "Random position is " + randomNewPos);
            //From the rest of the elements in the list, it will splice and inserted this same element
            ctx.trials.splice(randomNewPos, 0 , ctx.trials[ctx.cpt]);
          }
          d3.select("#placeholders").remove();
          nextTrial();
      }
    );
  }
}

var keyListener = function (event) {
  event.preventDefault();

  if (ctx.state == state.INSTRUCTIONS && event.code == "Enter") {

    d3.select("#instructions").remove();
    displayShapes();
  }

  if (ctx.state == state.SHAPES && event.code == "Space") {
    endTime = new Date();
    ctx.trials[ctx.cpt].searchTime = endTime.getTime() - ctx.startTime.getTime();

      displayPlaceholders();
    d3.select("#shapes").remove();
  }


}

var downloadLogs = function (event) {
  event.preventDefault();
  var csvContent = "data:text/csv;charset=utf-8,";
  console.log("logged lines count: " + ctx.loggedTrials.length);
  ctx.loggedTrials.forEach(function (rowArray) {
    var row = rowArray.join(",");
    csvContent += row + "\r\n";
    console.log(rowArray);
  });
  var encodedUri = encodeURI(csvContent);
  var downloadLink = d3.select("form")
    .append("a")
    .attr("href", encodedUri)
    .attr("download", "logs_" + ctx.trials[ctx.cpt][ctx.participantIndex] + "_" + Date.now() + ".csv")
    .text("logs_" + ctx.trials[ctx.cpt][ctx.participantIndex] + "_" + Date.now() + ".csv");
}


// returns an array of coordinates for laying out objectCount objects as a grid with an equal number of lines and columns
function gridCoordinates(objectCount, cellSize) {
  var gridSide = Math.sqrt(objectCount);
  var coords = [];
  for (var i = 0; i < objectCount; i++) {
    coords.push({
      x: i % gridSide * cellSize,
      y: Math.floor(i / gridSide) * cellSize
    });
  }
  return coords;
}

// shuffle the elements in the array
// copied from https://stackoverflow.com/questions/6274339/how-can-i-shuffle-an-array
function shuffle(array) {
  var j, x, i;
  for (i = array.length - 1; i > 0; i--) {
    j = Math.floor(Math.random() * (i + 1));
    x = array[i];
    array[i] = array[j];
    array[j] = x;
  }
  return array;
}

/*********************************************/

var createScene = function () {
  var svgEl = d3.select("#scene").append("svg");
  svgEl.attr("width", ctx.w);
  svgEl.attr("height", ctx.h)
    .classed("centered", true);

  loadData(svgEl);
};


/****************************************/
/******** STARTING PARAMETERS ***********/
/****************************************/

var setTrial = function (trialID) {
  ctx.startTrial = parseInt(trialID);
}

var setBlock = function (blockID) {
  ctx.startBlock = parseInt(blockID);

  var trial = "";
  var options = [];

  for (var i = 0; i < ctx.trials.length; i++) {
    if (ctx.trials[i][ctx.participantIndex] === ctx.participant) {
      if (parseInt(ctx.trials[i][ctx.blockIndex]) == ctx.startBlock) {
        if (!(ctx.trials[i][ctx.trialIndex] === trial)) {
          trial = ctx.trials[i][ctx.trialIndex];
          options.push(trial);
        }
      }
    }
  }

  var select = d3.select("#trialSel");

  select.selectAll("option")
    .data(options)
    .enter()
    .append("option")
    .text(function (d) { return d; });

  setTrial(options[0]);

}

var setParticipant = function (participantID) {
  ctx.participant = participantID;

  var block = "";
  var options = [];

  console.log(`ctx.blockIndex: ${ctx.blockIndex}`)

  for (var i = 0; i < ctx.trials.length; i++) {
    if (ctx.trials[i][ctx.participantIndex] === ctx.participant) {
      if (!(ctx.trials[i][ctx.blockIndex] === block)
        && (touchstone == 2 || ctx.trials[i][ctx.practiceIndex].toLowerCase() === "false")) {
        block = ctx.trials[i][ctx.blockIndex];
        options.push(block);
      }
    }
  }

  var select = d3.select("#blockSel")
  select.selectAll("option")
    .data(options)
    .enter()
    .append("option")
    .text(function (d) { return d; });

  setBlock(options[0]);

};

function onchangeParticipant() {
  selectValue = d3.select("#participantSel").property("value");
  setParticipant(selectValue);
};

function onchangeBlock() {
  selectValue = d3.select("#blockSel").property("value");
  setBlock(selectValue);
};

function onchangeTrial() {
  selectValue = d3.select("#trialSel").property("value");
  setTrial(selectValue);
};
