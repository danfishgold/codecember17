function parseMouseEvent(e, node) {
    return {
        pointers: [{ id: 'mouse', position: [e.pageX - node.offsetLeft, e.pageY - node.offsetTop] }],
        ctrlDown: e.ctrlKey
    }
}

function parseTouchEvent(e, node) {
    var pointers = [];
    for (var i = 0; i < e.changedTouches.length; i++) {
        var touch = e.changedTouches[i]
        pointers.push(parseTouch(touch, node))
    }
    return {
        pointers: pointers,
        ctrlDown: e.ctrlKey
    }
}

function parseTouch(touch, node) {
    return {
        id: touch.identifier.toString(),
        position: [touch.pageX - node.offsetLeft, touch.pageY - node.offsetTop],
    }
}

function addPointerEventListeners(app, node) {
    node.addEventListener('mousedown', function(e) {
        // console.log('mouse down', e)
        app.ports.mouseDown.send(parseMouseEvent(e, node))
        e.stopPropagation()
    })
    
    node.addEventListener('mousemove', function(e) {
        // console.log('mouse move', e)
        app.ports.mouseMove.send(parseMouseEvent(e, node))
        e.stopPropagation()
    })
    
    node.addEventListener('mouseup', function(e) {
        // console.log('mouse up', e)
        app.ports.mouseUp.send(parseMouseEvent(e, node))
        e.stopPropagation()
    })
    
    node.addEventListener('touchstart', function(e) {
        // console.log('touch start', e)
        app.ports.touchStart.send(parseTouchEvent(e, node))
        e.stopPropagation()
        e.preventDefault()
    })
    
    node.addEventListener('touchmove', function(e) {
        // console.log('touch move', e)
        app.ports.touchMove.send(parseTouchEvent(e, node))
        e.stopPropagation()
        e.preventDefault()
    })
    
    node.addEventListener('touchend', function(e) {
        // console.log('touch end', e)
        app.ports.touchEnd.send(parseTouchEvent(e, node))
        e.stopPropagation()
        e.preventDefault()
    })
    
    node.addEventListener('touchcancel', function(e) {
        // console.log('touch cancel', e)
        app.ports.touchCancel.send(parseTouchEvent(e, node))
        e.stopPropagation()
        e.preventDefault()
    })
}