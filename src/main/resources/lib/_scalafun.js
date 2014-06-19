function _sendRequestSync(url, class_path, fun_name) {
    return function () {
        var request = $.ajax({
            url: url,
            data: {
                object_name: class_path,
                function_name: fun_name,
                params: JSON.stringify(arguments)
            },
            async: false,
            global: false,
            dataType: "json",
            type: "POST"
        })

        if (request.status === 200 && request.readyState === 4) {
            try {
                // simulating Some(result)   
                var result = $.parseJSON(request.responseText);
                return {
                    "_cid": 0,
                    "_var0": result.result
                }
            } catch (e) {
                // simulating None
                return 1
            }
        } else {
            // simulating None
            return 1
        }
    }
}

function _sendRequestAsync(url, class_path, fun_name) {
    return function () {
        var fun = arguments[0] // 1. parameter is the callback function
        var scala_args = {} // holds the parameter for the scala function call
        for (var i = 1; i < arguments.length; i++) {
            scala_args[i - 1] = arguments[i]
        }

        var error_fun = function () {
            fun(1)() // simulating Option.None
        }
        var succ_fun = function (responseText, b) {
            // simulating Option.Some(result)   
            var some = {
                "_cid": 0,
                "_var0": responseText.result
            }
            fun(some)()
        }

        var request = $.ajax({
            url: url,
            data: {
                object_name: class_path,
                function_name: fun_name,
                params: JSON.stringify(scala_args)
            },
            success: succ_fun,
            error: error_fun,
            async: true,
            global: false,
            dataType: "json",
            type: "POST"
        })

        return;
    }
}
