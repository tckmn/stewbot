function showModal(contents, closable) {
    var overlay = document.getElementById('overlay');
    if (overlay) document.body.removeChild(overlay);

    overlay = document.createElement('div');
    overlay.id = 'overlay';
    document.body.appendChild(overlay);

    var box = document.createElement('div');
    box.appendChild(document.createTextNode(contents));
    if (closable) {
        box.classList.add('closable');
        box.addEventListener('click', function() {
            document.body.removeChild(overlay);
        });

        var info = document.createElement('p');
        info.appendChild(document.createTextNode('(click to close)'));
        box.appendChild(info);
    }
    overlay.appendChild(box);

    return overlay;
};
