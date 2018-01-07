function getSize(node) {
    return {
        width: node.clientWidth,
        height: node.clientHeight
    }
}
function addSizeEventListeners(app, node) {
    
    app.ports.getSize.subscribe(function() {
        app.ports.sizeChanges.send(getSize(node))
    })
    
    // https://developer.mozilla.org/en-US/docs/Web/Events/resize
    window.addEventListener('resize', resizeThrottler, false)

    var resizeTimeout;
    function resizeThrottler() {
        if (!resizeTimeout) {
            resizeTimeout = setTimeout(function() {
                resizeTimeout = null;
                app.ports.sizeChanges.send(getSize(node))
            }, 33);
        }
    }
}

