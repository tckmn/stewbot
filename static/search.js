window.addEventListener('load', function() {

    var submit = document.getElementById('submit');
    submit.addEventListener('click', function() {
        // disable ourselves
        submit.disabled = true;

        // obtain the data
        var val = document.getElementById('searcharea').value.replace(/\n+/, '\n').replace(/\n$/, ''),
            nitems = val.split('\n').length;

        // send the request
        var xhr = new XMLHttpRequest();
        xhr.open('POST', '/search', true);
        xhr.onload = function() {
            window.location.href = '/'+this.responseText;
        };
        xhr.send(val);

        // give some live updates
        var timeout = 500, fn = function() {
            var progress = new XMLHttpRequest();
            progress.open('POST', '/progress', true);
            progress.onload = function() {
                submit.textContent = (+this.responseText+1)+' of '+nitems+'...';
                setTimeout(fn, timeout);
            };
            progress.send();
        };
        setTimeout(fn, timeout);
    });

});
