fullscreen_tag1 = '
        .plot-zoom {
            position: absolute;
            border: none;
            background-color: transparent;
            bottom: 0;
            right: 0;

        }
        .full-screen {
            position: fixed;
            height: 98vh !important;
            width: 98vw !important;
            left: 0;
            top: 0;
            z-index: 9999;
            overflow: hidden;
        }
        '

fullscreen_tag2 = "
        function plotZoom(el){
            el = $(el);
            var parent = el.parent().parent();
            if(el.attr('data-full_screen') === 'false') {
                parent.addClass('full-screen').trigger('resize').fadeOut().fadeIn();
                el.attr('data-full_screen', 'true');
            } else {
                parent.removeClass('full-screen').trigger('resize').fadeOut().fadeIn();
                el.attr('data-full_screen', 'false');
            }
        }

        $(function(){
           $('.plotly-full-screen  .plotly.html-widget').append(
            `
            <div style='position: relative;'>
                <button onclick=plotZoom(this) class='plot-zoom' data-full_screen='false' title='Full screen'>
                    <i class='fa fa-expand-arrows-alt'></i>
                </button>
            </div>
            `);
        })
        "
usethis::use_data(fullscreen_tag1, overwrite = TRUE)
usethis::use_data(fullscreen_tag2, overwrite = TRUE)
