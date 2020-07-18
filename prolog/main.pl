:- module(main, [go/0]).
/** <module> Development Aid for declwsiplweb
 *
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:- html_resource(stylesheet, [virtual(true), requires(css('style.css'))]).
:- html_resource(bulma, [virtual(true),
                         requires('https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css')]).
:- html_resource(htmx, [virtual(true),
               %         requires('https://raw.githubusercontent.com/bigskysoftware/htmx/eb04ab5b3eafb6e03451a19f0da591c41e34a2f0/src/htmx.js')]).
                       requires('https://unpkg.com/htmx.org@0.0.7')]).
:- html_resource('https://unpkg.com/htmx.org@0.0.7', [mime_type(text/javascript)]).

:- http_handler(root('favicon.ico'), http_redirect(moved, img('favicon.ico')), [id(favicon)]).

:- http_handler(root(.), root_handler, [id(home)]).

:- use_module(library(settings)).

		 /*******************************
		 *           Static Handlers    *
		 *******************************/


:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(js, '/js', []).
http:location(img, '/icons', []).
http:location(css, '/css', []).

user:file_search_path(js, '../prolog/web/js').
user:file_search_path(css, '../prolog/web/css').
user:file_search_path(img, '../prolog/web/icons').

:- http_handler(js(.), http_reply_from_files(js(.), []), [prefix, id(js)]).
:- http_handler(css(.), http_reply_from_files(css(.), []), [prefix, id(css)]).
:- http_handler(img(.), http_reply_from_files(img(.), []), [prefix, id(img)]).

		 /*******************************
		 *  Start server
		 *******************************/

go :-
    load_settings('settings.db'),
    current_prolog_flag(version, X),
    X >= 80301,  % TODO temp til I figure out the crypto issue
    http_server(http_dispatch, [port(17100)]).
go :-
    current_prolog_flag(version, X),
    X < 80301,
    format('Need to be on SWI-Prolog 8.3.1 or better, you are on ~w~n', [X]).


		 /*******************************
		 *   Test Page
		 *******************************/

root_handler(_Request) :-
      reply_html_page(
          [title('declwebtest'), \html_receive(css)],
          \home_page).


home_page -->
    html([
        \html_requires(htmx),
        \html_requires(stylesheet),
        \basic_wrapper
    ]).

basic_wrapper -->
    html(div(button(['hx-post'('/clicked'),
                     'hx-swap'(outerHTML)],
                    'click me'))).
