// this file assumes a variable nodes is used to hold the nodes of the jstree
// and a <div> with id 'jstree' as a placeholder of the tree.

function getInputValue(id, defaultValue) {
	var value = $(id).val();
	if (value.length <= 0)
		value = defaultValue;

	return parseInt(value);
}

function findLevel(node) {
	var l = node.data.level - 1;
	if (typeof levels[l] == 'undefined')
		levels[l] = [];

	levels[l].push(node)

	$.each(node.children, function(i, v) {
		findLevel(v);
	})
}

// global variable
var levels = [];
var nodes = null;
var minYear = null;
var maxYear = null;

function generateTopicDocumentTable(documents, max) {
	var rows = [];
	$.each(documents, function(i, doc) {
		var paper = doc['paper'];
			if(paper['url'])
				rows.push("<tr><td>" + paper.series + "</td><td>" + paper.year
				+ "</td><td><a href=\"" + paper.url + "\">" + paper.title + "</a></td><td>" + paper.author + "</td><td>" + doc['prob'].toFixed(2)
				+ "</td></tr>");
			else
				rows.push("<tr><td>" + paper.series + "</td><td>" + paper.year
				+ "</td><td>" + paper.title + "</td><td>" + paper.author + "</td><td>" + doc['prob'].toFixed(2)
				+ "</td></tr>");
	})

	var table = $("<table class=\"tablesorter\"><thead><tr><th>Series</th><th>Year</th><th>Title</th><th>Author</th><th>Prob</th></tr></thead></table>")
			.append("<tbody/>").append(rows.join(""));

	table.tablesorter({
		theme : "bootstrap",
		widthFixed : true,
		headerTemplate : '{content} {icon}',
		widgets : [ "uitheme", "zebra" ],
		widgetOptions : {
			zebra : [ "even", "odd" ],
		}
	});

	return table;
}

function generateCountTable(documents) {
	var counts = {};
	for (var year = minYear; year <= maxYear; year++) {
		counts[year] = 0;
	}

	$.each(documents, function(i, doc) {
		counts[doc.paper.year]++;
	})

	var headRow = $("<tr/>");
	var bodyRow = $("<tr/>");
	for (var year = minYear; year <= maxYear; year++) {
		headRow.append("<th>" + year + "</th>");
		bodyRow.append("<td>" + counts[year] + "</td>");
	}
	var table = $("<table class=\"table table-bordered table-condensed\"/>")
			.append("<thead/>").append("<tbody/>");
	table.children("thead").append(headRow);
	table.children("tbody").append(bodyRow);

	return table;
}

function constructTree(n) {
	$("#jstree").on("changed.jstree", function(e, data) {
		// show a pop-up when a node has been selected
		if (data.action == "select_node") {
			$("#topic-modal-title").html(
				data.node.text + " (" + data.node.id + ")")

			$("#topic-modal-body").html("Loading...");

			$.ajax({   
				url: 'search/'+data.node.id,   
				type: "GET",   
				dataType: "json",   
				success: function(json) {
					max = 500;
					$("#topic-modal-body").html("");
					$("#topic-modal-body").append("<h5>Number of documents by year:</h5>");
					$("#topic-modal-body").append(generateCountTable(json.doc));
					$("#topic-modal-body").append("<h5>Document details (showing only the top " + max +"):</h5>");
					$("#topic-modal-body").append(generateTopicDocumentTable(json.doc, max));
				},
				error: function(jqXHR, textStatus, errorThrown) {
					$("#topic-modal-body").append("<p>Document information is not available.</p>")
				}
			});

			$("#topic-modal").modal()
		}
	}).jstree({
				"core" : {
					"data" : n,
					"themes" : {
						"icons" : false
					}
				},
				"search" : {
					"case_insensitive" : true,
					"show_only_matches" : true,
					"show_only_matches_children" : true
				},
				"plugins" : [ "search" ]
			});
}

// show the node within the specified range of levels.
// the nodes above the topmost level are discarded, while
// the nodes below the bottommost level are closed.
function showLevels(top, bottom) {
	var current = $('#jstree').jstree(true);
	if (typeof current != 'undefined' && current)
		current.destroy();

	for (var i = top; i > bottom; i--) {
		$.each(levels[i - 1], function(i, v) {
			v.state.opened = true;
		})
	}

	for (var i = bottom; i > 0; i--) {
		$.each(levels[i - 1], function(i, v) {
			v.state.opened = false;
		})
	}

	constructTree(levels[top - 1]);
}

function showAlert(message) {
	$("#alert-modal-message").html(message)
	$("#alert-modal").modal()
}

function loadTree(next){
	$.get({   
		url: 'sample.nodes.js',
		//dataType: "json",
		success: function(json) {
			//nodes = json['nodes'];
			// find the node levels in the tree
			$.each(nodes, function(i, v) {
				findLevel(v);
			});
			next();
		},
		error: function(jqXHR, textStatus, errorThrown) {
			$("#jstree").append("<p>"+errorThrown+"</p>");
		}
	});
}

function loadMeta(next){
	$.get({   
		url: 'sample.meta.json',  
		dataType: "json",
		success: function(json) {
			minYear = json.minYear;
			maxYear = json.maxYear;
			next();
		},
		error: function(jqXHR, textStatus, errorThrown) {
			$("#jstree").append("<p>"+errorThrown+"</p>");
		}
	});
}

function initialize(){
	topmost = levels.length
	bottommost = Math.max(1, levels.length - 1)

	// set the default values of the levels
	$("#top-input").val(topmost)
	$("#bottom-input").val(bottommost)

	$('[data-toggle="tooltip"]').tooltip()

	showLevels(topmost, bottommost);

	$('#level-button').click(function() {
		var top = getInputValue('#top-input', 1000000)
		var bottom = getInputValue('#bottom-input', 1)

		if (top > levels.length) {
			showAlert("The topmost level (left) cannot be larger than "
					+ levels.length + ".")
			$("#top-input").val(topmost)
		} else if (bottom < 1) {
			showAlert("The bottommost level (right) cannot be smaller than 1.")
			$("#bottom-input").val(1)
		} else if (top < bottom) {
			showAlert("The topmost level (left) cannot be smaller than the bottommost level (right).")
		} else {
			showLevels(top, bottom);
		}
	})

	$("#filter-button").click(function() {
		var searchString = $("#search-input").val();
		$('#jstree').jstree('search', searchString);
	});

	$("#clear-button").click(function() {
		$('#jstree').jstree(true).clear_search();
		$("#search-input").val("");
	});

	$.tablesorter.themes.bootstrap = {
		table : 'table table-bordered table-hover',
		caption : 'caption',
		header : 'bootstrap-header', 
		sortNone : '',
		sortAsc : '',
		sortDesc : '',
		active : '', 
		hover : '', 
		icons : '', 
		iconSortNone : 'bootstrap-icon-unsorted',
		iconSortAsc : 'glyphicon glyphicon-chevron-up',
		iconSortDesc : 'glyphicon glyphicon-chevron-down',
		filterRow : '', 
		footerRow : '',
		footerCells : '',
		even : '', 
		odd : '' 
	};

//	$('#topic-modal').on('hidden.bs.modal', function (e) {
//    	$("#jstree").focus()
//    })
}

loadTree(function(){
	loadMeta(function(){
		initialize();
	});
});
