var app = Elm.Main.fullscreen();
app.ports.notify.subscribe(function(content) {
    $.Notify({
        content: content,
        type: 'alert',
        keepOpen: true
    });
});
