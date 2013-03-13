$(function() {
    function renderError(msg, e) {
        console.log(msg, e);

        $("#errors").toggle();
        $("#error-msg").html(msg);
    }

    function loadDictionaries() {
        $.get("/list", function(data) {
            if (!data || !data.dictionaries) {
                renderError("Unable to load dictionaries", data);
                return;
            };

            $.each(data.dictionaries, function(i, dict) {
                var opt = "<option value='" + dict.id + "'>" + dict.name + "</option>";

                $("#dictionary").append(opt);
            });
        });
    };

    function displayConcepts(concepts) {
        var html = "<ul>";

        $.each(concepts, function(j, concept) {
            html += "<li>";
            html += concept.prefLabel.name;
            html += "</li>";
        });

        html += "</ul>";
        $("#vis").html(html);
    }

    function extractText() {
        var projectId = $("#dictionary :selected").val();
        var language = $("#language :selected").val();
        var text = $("#text").val();

        var query =  "/extract?id=" + projectId + "&language=" + language + "&text=" + text;

        $.get(query, function(data) {
            if (!data || !data.extraction) {
                renderError("Unable to extract text", data);
                return;
            }

            $.each(data.extraction.annotations, function(i, annotation) {
                displayConcepts(annotation.concepts);
            });
        });
    };

    function enableDictionaryMode() {
        $("#row-extract").hide();
        $("#row-dictionary").show();
    }

    function enableExtractMode() {
        $("#row-dictionary").hide();
        $("#row-extract").show();
    }

    loadDictionaries();

    $("#extract-mode").click(function() {
        enableExtractMode(); 
    });

    $("#dict-mode").click(function() {
        enableDictionaryMode();
    });

    $("#submit").click(function() {
        extractText();
        return false;
    });
});
