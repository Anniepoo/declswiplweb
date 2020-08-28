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
    html(div([\do_button(1), \do_button(2), \do_button(3)])).


do_button(N) -->
      {
    get_time(Now),
    format_time(atom(Time),
                ' %A  %H%M:%S', Now)
      },
      htmx(button(['hx-post'('/clicked'),
                   style('background-color: #FF0000;'),
                     'hx-swap'(outerHTML)],
                  [ 'Number', N, 'last clicked at', Time])),
      !.   % DEBUG
do_button(_) -->
    { gtrace }.

% there's an inherent diff on the back end between loading, which should
% not modify state, and clicking on the button, which could. May need
% a special htmx_cmd flag that says this has to  be handled differently.

% think through access
%
% makes debugging from traffic capture harder

% do_button(hi)
% needs  'hx-vars'('greet:"~w"'-[Greeting]),
%
%
% 1. htmx//1 inspects the termerized htmx for px-post.
% 2. htmx//1 checks to see if there's already a matching handler with
% the htmx_handler flag in the options
% 3. if not, it creates the handler
% 4. It needs 2 pieces of info to creat the handler - the path and the'
% inclusion name. The path it gets from px-post.  The inclusion name is
% the inclusion it's in, which it gets by stack inspection

% bet you have module issues with the handler & inclusion
%
		 /**********************************
		 *    htmx handler for the button *
		 *********************************/

% :- http_handler(root(clicked), htmx_handler(do_button),
% [id(clicked)]).

htmx_handler(Inclusion, _Request) :-
      within_htmx(
          phrase(html(\Inclusion), Tokens)
      ),
      format('Content-type: text/html~n~n'),
      print_html(Tokens).


within_htmx(Goal) :-
    setup_call_cleanup(
        b_setval(htmx, true),
        call(Goal),
        b_setval(htmx,false)
    ).

:- dynamic http_dispatch:handler/4.                   % Path, Action, IsPrefix, Options
:- multifile http_dispatch:handler/4.

:-html_meta htmx(html, ?, ?).

% limit - no semantic args to Spec
% limit - hx-post only
% limit - hx-post must be in outermost element
htmx(Spec) -->
    { b_setval(htmx, false) },
    { plain_spec_params_spec(Spec, PSpec) },
    html(PSpec),
    {
        sub_term('hx-post'(Path), Spec),
        prolog_current_frame(HTMXDCG),
        prolog_frame_attribute(HTMXDCG, parent, Parent),
        prolog_frame_attribute(Parent, predicate_indicator, Functor/2), %temp
        % is it dispatched via a handler with htmx_handler(true)?
        (   http_dispatch:handler(Path,
                                    main:htmx_handler(Functor),
                                    _IsPrefix,
                                    Options),
            memberchk(htmx_handler(true), Options)
        ;
            http_handler(Path, htmx_handler(Functor), [htmx_handler(true)])
        )
    }.

plain_spec_params_spec(Spec, Spec) :-
    functor(Spec, _, 0).
plain_spec_params_spec(Spec, PSpec) :-
    Spec =.. [Functor | Args],
    Args = [_ | _],








