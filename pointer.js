function parseMouseEvent(e) {
    return {
        pointers: [{ id: 'mouse', position: [e.clientX, e.clientY] }],
        ctrlDown: e.ctrlKey
    }
}

function parseTouchEvent(e) {
    var pointers = [];
    for (var touch of e.changedTouches) {
        pointers.push(parseTouch(touch))
    }
    return {
        pointers: pointers,
        ctrlDown: e.ctrlKey
    }
}

function parseTouch(touch) {
    return {
        id: touch.identifier.toString(),
        position: [touch.clientX, touch.clientY],
    }
}

function addPointerEventListeners(app, node) {
    node.addEventListener('mousedown', function(e) {
        // console.log('mouse down', e)
        app.ports.mouseDown.send(parseMouseEvent(e))
        e.stopPropagation()
    })
    
    node.addEventListener('mousemove', function(e) {
        // console.log('mouse move', e)
        app.ports.mouseMove.send(parseMouseEvent(e))
        e.stopPropagation()
    })
    
    node.addEventListener('mouseup', function(e) {
        // console.log('mouse up', e)
        app.ports.mouseUp.send(parseMouseEvent(e))
        e.stopPropagation()
    })
    
    node.addEventListener('touchstart', function(e) {
        // console.log('touch start', e)
        app.ports.touchStart.send(parseTouchEvent(e))
        e.stopPropagation()
        e.preventDefault()
    })
    
    node.addEventListener('touchmove', function(e) {
        // console.log('touch move', e)
        app.ports.touchMove.send(parseTouchEvent(e))
        e.stopPropagation()
        e.preventDefault()
    })
    
    node.addEventListener('touchend', function(e) {
        // console.log('touch end', e)
        app.ports.touchEnd.send(parseTouchEvent(e))
        e.stopPropagation()
        e.preventDefault()
    })
    
    node.addEventListener('touchcancel', function(e) {
        // console.log('touch cancel', e)
        app.ports.touchCancel.send(parseTouchEvent(e))
        e.stopPropagation()
        e.preventDefault()
    })
}