window.addEventListener('load', function() {

    Array.from(document.querySelectorAll('.add')).map(function(add) {
        add.addEventListener('click', function(e) {
            e.preventDefault();
            showModal('adding to cart...', false);

            var xhr = new XMLHttpRequest();
            xhr.open('POST', add.href, true);
            xhr.onload = function() {
                showModal('done!', true);
            };
            xhr.send();
        });
    });

});
