window.addEventListener('load', function() {

    var submit = document.getElementById('submit');
    submit.addEventListener('click', function() {
        // disable ourselves
        submit.disabled = true;

        // send the request
        var xhr = new XMLHttpRequest();
        xhr.open('POST', '/search', true);
        xhr.onload = function() {
            console.log(this.responseText);
        };
        xhr.send(document.getElementById('searcharea').value);

        // give some live updates
        var timeout = 500, fn = function() {
            var progress = new XMLHttpRequest();
            progress.open('POST', '/progress', true);
            progress.onload = function() {
                console.log(this.responseText);
                setTimeout(fn, timeout);
            };
            progress.send();
        };
        setTimeout(fn, timeout);
    });

});
