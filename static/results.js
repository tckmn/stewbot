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
            case 'Backspace':
            case 'Delete':
            case 'd':
            case 'x':
                // TODO tell the server to delet
                break;
            case 'Escape':
                // TODO e x p a n d the current item
                break;
            default: return;
        }
        e.preventDefault();
    });

});
