/**
@Deprecated
*/
function run_mmt() {
    $.ajax({
        type: "POST",
        url: "http://localhost:8080/:post",
        data: {
            'body': "namespace http://cds.omdoc.org/assessment\nview Inst : ?Profit -> ?Lists = \npi = [ 2, 5, 6 ]\nbase = 2\n",
            'format': 'mmt',
            'dpath': '/home/assessment/'
        },
        //		dataType : 'json',
        complete: function(r, s) {
            console.log(s);
        },
        success: function(result) {
            console.log(result);
            $.ajax({
                type: "POST",
                url: "http://localhost:8080/:frameit/get",
                data: {
                    'viewPath': "http://cds.omdoc.org/assessment?Inst",
                    'solPath': "http://cds.omdoc.org/assessment?Assessment?assess"
                },
                //				dataType : 'json',
                success: function(result) {
                    console.log(result);

                }
            });
        }
    });
}

function gen_comparator_view(next_func) {
    $.ajax({
        type: "POST",
        url: "http://localhost:8080/:post",
        data: {
            'body': "namespace http://cds.omdoc.org/assessment\nview ComparationInst : ?Comparation -> ?Lists = \nbase = 0\ncomp = gt\n",
            'format': 'mmt',
            'dpath': '/home/assessment/'
        },
        //      dataType : 'json',
        complete: function(r, s) {
            console.log("Completed comparator view" + s);
        },
        success: function(result) {
            console.log(result);
            next_func();
        }
    });
}

function profit_view(next_func) {
    $.ajax({
        type: "POST",
        url: "http://localhost:8080/:post",
        data: {
            'body': "namespace http://cds.omdoc.org/assessment\nview ProfitInst : ?ValuationFunction -> ?Profit = \nfunction = pi\nvalue = x\n",
            'format': 'mmt',
            'dpath': '/home/assessment/'
        },
        //      dataType : 'json',
        complete: function(r, s) {
            console.log("Completed profit view" + s);
        },
        success: function(result) {
            console.log(result);
            next_func();
        }
    });
}


function concrete_profit(next_func) {
    $.ajax({
        type: "POST",
        url: "http://localhost:8080/:post",
        data: {
            'body': "namespace http://cds.omdoc.org/assessment\nview ConcreteProfit : ?Profit -> ?Lists = \n include ?Comparation = ?ComparationInst\npi = [8,10,12]\nx = 2\n",
            'format': 'mmt',
            'dpath': '/home/assessment/'
        },
        //      dataType : 'json',
        complete: function(r, s) {
            console.log(s);
        },
        success: function(result) {
            console.log(result);
            next_func();
        }
    });
}

function push_out_profit() {
    $.ajax({
        type: "POST",
        url: "http://localhost:8080/:frameit/get",
        data: {
            'viewPath': "http://cds.omdoc.org/assessment?ProfitInst http://cds.omdoc.org/assessment?ConcreteProfit",
            'solPath': "http://cds.omdoc.org/assessment?Assessment?assess_value"
        },
        //              dataType : 'json',
        success: function(result) {
            console.log(result);

        }
    });
}

function test() {
    gen_comparator_view(function() {
        profit_view(function() {
            concrete_profit(push_out_profit);
        });
    });
}