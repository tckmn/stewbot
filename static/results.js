window.addEventListener('load', function() {

    var active = document.querySelector('div.chosen'),
        activeIdx = 0,
        cols = document.querySelectorAll('div.items');
    active.classList.add('active');

    var updateActive = function() {
        active.classList.remove('active');
        active = cols[activeIdx].querySelector('.chosen');
        active.classList.add('active');
        document.body.scrollTo(active.offsetLeft + active.offsetWidth/2 - window.innerWidth/2, 0);
        vertScroll();
    };

    var tryMove = function(e) {
        if (e && e.classList.contains('item')) {
            active.classList.remove('active');
            active.classList.remove('chosen');
            active = e;
            active.classList.add('active');
            active.classList.add('chosen');
            vertScroll();
        }
    };

    var vertScroll = function() {
        var p = active.parentNode,
            pos = p.scrollTop,
            y = active.offsetTop - p.offsetTop;
        pos = Math.min(pos, y);
        pos = Math.max(pos, y - p.offsetHeight + active.offsetHeight);
        p.scrollTo(0, pos);
    };

    var go = function() {
        var bad = false, data = Array.from(document.querySelectorAll('.chosen'))
            .map(function(x) {
                var id = x.dataset.id,
                    quant = x.parentNode.parentNode.querySelector('.quant').textContent;
                if (!bad && quant === '') showModal('at least 1 item is missing quantity', bad=true);
                else if (quant === '0') return '';
                else return id + ' ' + quant;
            }).join('\n').replace(/\n+/g, '\n');
        if (bad) return;

        showModal('saving order...', false);

        var xhr = new XMLHttpRequest();
        xhr.open('POST', '/save'+location.pathname.match(/.\d[^.]*/)[0], true);
        xhr.onload = function() {
            showModal('done!', true);
        };
        xhr.send(data);
    };

    var typeQuant = function(k) {
        var quant = cols[activeIdx].querySelector('.quant');
        if (k == 'Backspace') quant.textContent = quant.textContent.slice(0, -1);
        else quant.textContent = quant.textContent + k;
    };

    window.addEventListener('keydown', function(e) {
        switch (e.key) {
            case 'ArrowLeft':
            case 'h':
                if (activeIdx) updateActive(--activeIdx);
                break;
            case 'ArrowRight':
            case 'l':
                if (activeIdx < cols.length - 1) updateActive(++activeIdx);
                break;
            case 'ArrowUp':
            case 'k':
                tryMove(active.previousSibling);
                break;
            case 'ArrowDown':
            case 'j':
                tryMove(active.nextSibling);
                break;
            case 'PageUp':
            case 'g':
                tryMove(active.parentNode.firstChild);
                break;
            case 'PageDown':
            case 'G':
                tryMove(active.parentNode.lastChild);
                break;
            case 'Delete':
            case 'd':
            case 'x':
                // TODO tell the server to delet
                break;
            case 'Escape':
            case 'Enter':
                // TODO e x p a n d the current item
                break;
            case '`':
                go();
                break;
            case '0': case '1': case '2': case '3': case '4': case 'Backspace':
            case '5': case '6': case '7': case '8': case '9': case '.':
                typeQuant(e.key);
                break;
            default: return;
        }
        e.preventDefault();
    });

});
