{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Framework.Plzwrk.Tag
Description : Base functions for plzwrk
Copyright   : (c) Mike Solomon 2020
License     : GPL-3
Maintainer  : mike@meeshkan.com
Stability   : experimental
Portability : POSIX, Windows

This module contains tags for web development. It has stuff like
@img@, @div@, @br@, @span@, etc. Because the module is huge, we
recommend doing selective import of the tags you need.

There are three conventions for tag naming:

* tags that accept children, like @div@ and @p@
* tags that do not have children but could have attributes, like @img@
* tags that have no attributes and no children, like @br@

For tags that can have children, the following six tags are exported
(we'll use @div@ as an example, but the same works for @span@, @section@ etc):

* @div@ : A div that does not need to be hydrated with a state.
* @div'@ : A div that is hydrated with a state.
* @div_@ : A div with no attributes that does not need to be hydrated with a state.
* @div'_@ : A div with no attributes that is hydrated with a state.
* @div__@ : A div that only contains text that does not need to be hydrated with a state.
* @div'__@ : A div that only contains text that is hydrated with a state.

For tags that do not have children, the following six tags are exported
(we'll use @img@ as an example):

* @img@ : A div that does not need to be hydrated with a state.
* @img'@ : A div that is hydrated with a state.
* @img_@ : A div with no attributes that does not need to be hydrated with a state.
* @img'_@ : A div with no attributes that is hydrated with a state.

For tags like br, there is only one export, namely @br@.

Here are some gotchyas to bear in mind:

* The HTML @data@ tag is renamed to @_data@ here.
* Due to the volume of tags in this module, some of
  them follow an incorrect convention, ie accepting children
  when they shouldn't be able to. If you spot one, please
  make a PR.
-}
module Web.Framework.Plzwrk.Tag( 
a
, a'
, a_
, a'_
, a__
, a'__
, abbr
, abbr'
, abbr_
, abbr'_
, abbr__
, abbr'__
, acronym
, acronym'
, acronym_
, acronym'_
, acronym__
, acronym'__
, address
, address'
, address_
, address'_
, address__
, address'__
, applet
, applet'
, applet_
, applet'_
, applet__
, applet'__
, area
, area'
, area_
, area'_
, area__
, area'__
, article
, article'
, article_
, article'_
, article__
, article'__
, aside
, aside'
, aside_
, aside'_
, aside__
, aside'__
, audio
, audio'
, audio_
, audio'_
, audio__
, audio'__
, b
, b'
, b_
, b'_
, b__
, b'__
, base
, base'
, base_
, base'_
, base__
, base'__
, basefont
, basefont'
, basefont_
, basefont'_
, basefont__
, basefont'__
, bdi
, bdi'
, bdi_
, bdi'_
, bdi__
, bdi'__
, bdo
, bdo'
, bdo_
, bdo'_
, bdo__
, bdo'__
, big
, big'
, big_
, big'_
, big__
, big'__
, blockquote
, blockquote'
, blockquote_
, blockquote'_
, blockquote__
, blockquote'__
, body
, body'
, body_
, body'_
, body__
, body'__
, br
, button
, button'
, button_
, button'_
, button__
, button'__
, canvas
, canvas'
, canvas_
, canvas'_
, canvas__
, canvas'__
, caption
, caption'
, caption_
, caption'_
, caption__
, caption'__
, center
, center'
, center_
, center'_
, center__
, center'__
, cite
, cite'
, cite_
, cite'_
, cite__
, cite'__
, code
, code'
, code_
, code'_
, code__
, code'__
, col
, col'
, col_
, col'_
, col__
, col'__
, colgroup
, colgroup'
, colgroup_
, colgroup'_
, colgroup__
, colgroup'__
, _data
, _data'
, _data_
, _data'_
, _data__
, _data'__
, _datalist
, _datalist'
, _datalist_
, _datalist'_
, _datalist__
, _datalist'__
, dd
, dd'
, dd_
, dd'_
, dd__
, dd'__
, del
, del'
, del_
, del'_
, del__
, del'__
, details
, details'
, details_
, details'_
, details__
, details'__
, dfn
, dfn'
, dfn_
, dfn'_
, dfn__
, dfn'__
, dialog
, dialog'
, dialog_
, dialog'_
, dialog__
, dialog'__
, dir
, dir'
, dir_
, dir'_
, dir__
, dir'__
, div
, div'
, div_
, div'_
, div__
, div'__
, dl
, dl'
, dl_
, dl'_
, dl__
, dl'__
, dt
, dt'
, dt_
, dt'_
, dt__
, dt'__
, em
, em'
, em_
, em'_
, em__
, em'__
, embed
, embed'
, embed_
, embed'_
, embed__
, embed'__
, fieldset
, fieldset'
, fieldset_
, fieldset'_
, fieldset__
, fieldset'__
, figcaption
, figcaption'
, figcaption_
, figcaption'_
, figcaption__
, figcaption'__
, figure
, figure'
, figure_
, figure'_
, figure__
, figure'__
, font
, font'
, font_
, font'_
, font__
, font'__
, footer
, footer'
, footer_
, footer'_
, footer__
, footer'__
, form
, form'
, form_
, form'_
, form__
, form'__
, frame
, frame'
, frame_
, frame'_
, frame__
, frame'__
, frameset
, frameset'
, frameset_
, frameset'_
, frameset__
, frameset'__
, h1
, h1'
, h1_
, h1'_
, h1__
, h1'__
, h2
, h2'
, h2_
, h2'_
, h2__
, h2'__
, h3
, h3'
, h3_
, h3'_
, h3__
, h3'__
, h4
, h4'
, h4_
, h4'_
, h4__
, h4'__
, h5
, h5'
, h5_
, h5'_
, h5__
, h5'__
, h6
, h6'
, h6_
, h6'_
, h6__
, h6'__
, head
, head'
, head_
, head'_
, head__
, head'__
, header
, header'
, header_
, header'_
, header__
, header'__
, hr
, html
, html'
, html_
, html'_
, html__
, html'__
, i
, i'
, i_
, i'_
, i__
, i'__
, iframe
, iframe'
, iframe_
, iframe'_
, iframe__
, iframe'__
, img
, img'
, img_
, img'_
, input
, input'
, input_
, input'_
, input__
, input'__
, ins
, ins'
, ins_
, ins'_
, ins__
, ins'__
, kbd
, kbd'
, kbd_
, kbd'_
, kbd__
, kbd'__
, label
, label'
, label_
, label'_
, label__
, label'__
, legend
, legend'
, legend_
, legend'_
, legend__
, legend'__
, li
, li'
, li_
, li'_
, li__
, li'__
, link
, link'
, link_
, link'_
, link__
, link'__
, main
, main'
, main_
, main'_
, main__
, main'__
, map
, map'
, map_
, map'_
, map__
, map'__
, mark
, mark'
, mark_
, mark'_
, mark__
, mark'__
, meta
, meta'
, meta_
, meta'_
, meta__
, meta'__
, meter
, meter'
, meter_
, meter'_
, meter__
, meter'__
, nav
, nav'
, nav_
, nav'_
, nav__
, nav'__
, noframes
, noframes'
, noframes_
, noframes'_
, noframes__
, noframes'__
, noscript
, noscript'
, noscript_
, noscript'_
, noscript__
, noscript'__
, object
, object'
, object_
, object'_
, object__
, object'__
, ol
, ol'
, ol_
, ol'_
, ol__
, ol'__
, optgroup
, optgroup'
, optgroup_
, optgroup'_
, optgroup__
, optgroup'__
, option
, option'
, option_
, option'_
, option__
, option'__
, output
, output'
, output_
, output'_
, output__
, output'__
, p
, p'
, p_
, p'_
, p__
, p'__
, param
, param'
, param_
, param'_
, param__
, param'__
, picture
, picture'
, picture_
, picture'_
, picture__
, picture'__
, pre
, pre'
, pre_
, pre'_
, pre__
, pre'__
, progress
, progress'
, progress_
, progress'_
, progress__
, progress'__
, q
, q'
, q_
, q'_
, q__
, q'__
, rp
, rp'
, rp_
, rp'_
, rp__
, rp'__
, rt
, rt'
, rt_
, rt'_
, rt__
, rt'__
, ruby
, ruby'
, ruby_
, ruby'_
, ruby__
, ruby'__
, s
, s'
, s_
, s'_
, s__
, s'__
, samp
, samp'
, samp_
, samp'_
, samp__
, samp'__
, script
, script'
, script_
, script'_
, script__
, script'__
, section
, section'
, section_
, section'_
, section__
, section'__
, select
, select'
, select_
, select'_
, select__
, select'__
, small
, small'
, small_
, small'_
, small__
, small'__
, source
, source'
, source_
, source'_
, source__
, source'__
, span
, span'
, span_
, span'_
, span__
, span'__
, strike
, strike'
, strike_
, strike'_
, strike__
, strike'__
, strong
, strong'
, strong_
, strong'_
, strong__
, strong'__
, style
, style'
, style_
, style'_
, style__
, style'__
, sub
, sub'
, sub_
, sub'_
, sub__
, sub'__
, summary
, summary'
, summary_
, summary'_
, summary__
, summary'__
, sup
, sup'
, sup_
, sup'_
, sup__
, sup'__
, svg
, svg'
, svg_
, svg'_
, svg__
, svg'__
, table
, table'
, table_
, table'_
, table__
, table'__
, tbody
, tbody'
, tbody_
, tbody'_
, tbody__
, tbody'__
, td
, td'
, td_
, td'_
, td__
, td'__
, template
, template'
, template_
, template'_
, template__
, template'__
, textarea
, textarea'
, textarea_
, textarea'_
, textarea__
, textarea'__
, tfoot
, tfoot'
, tfoot_
, tfoot'_
, tfoot__
, tfoot'__
, th
, th'
, th_
, th'_
, th__
, th'__
, thead
, thead'
, thead_
, thead'_
, thead__
, thead'__
, time
, time'
, time_
, time'_
, time__
, time'__
, title
, title'
, title_
, title'_
, title__
, title'__
, tr
, tr'
, tr_
, tr'_
, tr__
, tr'__
, track
, track'
, track_
, track'_
, track__
, track'__
, tt
, tt'
, tt_
, tt'_
, tt__
, tt'__
, u
, u'
, u_
, u'_
, u__
, u'__
, ul
, ul'
, ul_
, ul'_
, ul__
, ul'__
, var
, var'
, var_
, var'_
, var__
, var'__
, video
, video'
, video_
, video'_
, video__
, video'__
, wbr
, txt
, txt'
) where

import Prelude(String)
import Web.Framework.Plzwrk.Base

type AFSig s opq
  = (s -> Attributes s opq) -> [s -> Node s opq] -> (s -> Node s opq)
type Sig s opq = (s -> Attributes s opq) -> [s -> Node s opq] -> Node s opq

type AFSig_ s opq = [s -> Node s opq] -> (s -> Node s opq)
type Sig_ s opq = [s -> Node s opq] -> Node s opq

type AFSig__ s opq = String -> (s -> Node s opq)
type Sig__ s opq = String -> Node s opq


a :: AFSig s opq
a x y = (\_ -> Element "a" x y)

a' :: Sig s opq
a' = Element "a"

a_ :: AFSig_ s opq
a_ x = (\_ -> Element "a" dats x)

a'_ :: Sig_ s opq
a'_ x = Element "a" dats x

a__ :: AFSig__ s opq
a__ x = (\_ -> Element "a" dats [txt x])

a'__ :: Sig__ s opq
a'__ x = Element "a" dats [txt x]


abbr :: AFSig s opq
abbr x y = (\_ -> Element "abbr" x y)

abbr' :: Sig s opq
abbr' = Element "abbr"

abbr_ :: AFSig_ s opq
abbr_ x = (\_ -> Element "abbr" dats x)

abbr'_ :: Sig_ s opq
abbr'_ x = Element "abbr" dats x

abbr__ :: AFSig__ s opq
abbr__ x = (\_ -> Element "abbr" dats [txt x])

abbr'__ :: Sig__ s opq
abbr'__ x = Element "abbr" dats [txt x]


acronym :: AFSig s opq
acronym x y = (\_ -> Element "acronym" x y)

acronym' :: Sig s opq
acronym' = Element "acronym"

acronym_ :: AFSig_ s opq
acronym_ x = (\_ -> Element "acronym" dats x)

acronym'_ :: Sig_ s opq
acronym'_ x = Element "acronym" dats x

acronym__ :: AFSig__ s opq
acronym__ x = (\_ -> Element "acronym" dats [txt x])

acronym'__ :: Sig__ s opq
acronym'__ x = Element "acronym" dats [txt x]


address :: AFSig s opq
address x y = (\_ -> Element "address" x y)

address' :: Sig s opq
address' = Element "address"

address_ :: AFSig_ s opq
address_ x = (\_ -> Element "address" dats x)

address'_ :: Sig_ s opq
address'_ x = Element "address" dats x

address__ :: AFSig__ s opq
address__ x = (\_ -> Element "address" dats [txt x])

address'__ :: Sig__ s opq
address'__ x = Element "address" dats [txt x]


applet :: AFSig s opq
applet x y = (\_ -> Element "applet" x y)

applet' :: Sig s opq
applet' = Element "applet"

applet_ :: AFSig_ s opq
applet_ x = (\_ -> Element "applet" dats x)

applet'_ :: Sig_ s opq
applet'_ x = Element "applet" dats x

applet__ :: AFSig__ s opq
applet__ x = (\_ -> Element "applet" dats [txt x])

applet'__ :: Sig__ s opq
applet'__ x = Element "applet" dats [txt x]


area :: AFSig s opq
area x y = (\_ -> Element "area" x y)

area' :: Sig s opq
area' = Element "area"

area_ :: AFSig_ s opq
area_ x = (\_ -> Element "area" dats x)

area'_ :: Sig_ s opq
area'_ x = Element "area" dats x

area__ :: AFSig__ s opq
area__ x = (\_ -> Element "area" dats [txt x])

area'__ :: Sig__ s opq
area'__ x = Element "area" dats [txt x]


article :: AFSig s opq
article x y = (\_ -> Element "article" x y)

article' :: Sig s opq
article' = Element "article"

article_ :: AFSig_ s opq
article_ x = (\_ -> Element "article" dats x)

article'_ :: Sig_ s opq
article'_ x = Element "article" dats x

article__ :: AFSig__ s opq
article__ x = (\_ -> Element "article" dats [txt x])

article'__ :: Sig__ s opq
article'__ x = Element "article" dats [txt x]


aside :: AFSig s opq
aside x y = (\_ -> Element "aside" x y)

aside' :: Sig s opq
aside' = Element "aside"

aside_ :: AFSig_ s opq
aside_ x = (\_ -> Element "aside" dats x)

aside'_ :: Sig_ s opq
aside'_ x = Element "aside" dats x

aside__ :: AFSig__ s opq
aside__ x = (\_ -> Element "aside" dats [txt x])

aside'__ :: Sig__ s opq
aside'__ x = Element "aside" dats [txt x]


audio :: AFSig s opq
audio x y = (\_ -> Element "audio" x y)

audio' :: Sig s opq
audio' = Element "audio"

audio_ :: AFSig_ s opq
audio_ x = (\_ -> Element "audio" dats x)

audio'_ :: Sig_ s opq
audio'_ x = Element "audio" dats x

audio__ :: AFSig__ s opq
audio__ x = (\_ -> Element "audio" dats [txt x])

audio'__ :: Sig__ s opq
audio'__ x = Element "audio" dats [txt x]


b :: AFSig s opq
b x y = (\_ -> Element "b" x y)

b' :: Sig s opq
b' = Element "b"

b_ :: AFSig_ s opq
b_ x = (\_ -> Element "b" dats x)

b'_ :: Sig_ s opq
b'_ x = Element "b" dats x

b__ :: AFSig__ s opq
b__ x = (\_ -> Element "b" dats [txt x])

b'__ :: Sig__ s opq
b'__ x = Element "b" dats [txt x]


base :: AFSig s opq
base x y = (\_ -> Element "base" x y)

base' :: Sig s opq
base' = Element "base"

base_ :: AFSig_ s opq
base_ x = (\_ -> Element "base" dats x)

base'_ :: Sig_ s opq
base'_ x = Element "base" dats x

base__ :: AFSig__ s opq
base__ x = (\_ -> Element "base" dats [txt x])

base'__ :: Sig__ s opq
base'__ x = Element "base" dats [txt x]


basefont :: AFSig s opq
basefont x y = (\_ -> Element "basefont" x y)

basefont' :: Sig s opq
basefont' = Element "basefont"

basefont_ :: AFSig_ s opq
basefont_ x = (\_ -> Element "basefont" dats x)

basefont'_ :: Sig_ s opq
basefont'_ x = Element "basefont" dats x

basefont__ :: AFSig__ s opq
basefont__ x = (\_ -> Element "basefont" dats [txt x])

basefont'__ :: Sig__ s opq
basefont'__ x = Element "basefont" dats [txt x]


bdi :: AFSig s opq
bdi x y = (\_ -> Element "bdi" x y)

bdi' :: Sig s opq
bdi' = Element "bdi"

bdi_ :: AFSig_ s opq
bdi_ x = (\_ -> Element "bdi" dats x)

bdi'_ :: Sig_ s opq
bdi'_ x = Element "bdi" dats x

bdi__ :: AFSig__ s opq
bdi__ x = (\_ -> Element "bdi" dats [txt x])

bdi'__ :: Sig__ s opq
bdi'__ x = Element "bdi" dats [txt x]


bdo :: AFSig s opq
bdo x y = (\_ -> Element "bdo" x y)

bdo' :: Sig s opq
bdo' = Element "bdo"

bdo_ :: AFSig_ s opq
bdo_ x = (\_ -> Element "bdo" dats x)

bdo'_ :: Sig_ s opq
bdo'_ x = Element "bdo" dats x

bdo__ :: AFSig__ s opq
bdo__ x = (\_ -> Element "bdo" dats [txt x])

bdo'__ :: Sig__ s opq
bdo'__ x = Element "bdo" dats [txt x]


big :: AFSig s opq
big x y = (\_ -> Element "big" x y)

big' :: Sig s opq
big' = Element "big"

big_ :: AFSig_ s opq
big_ x = (\_ -> Element "big" dats x)

big'_ :: Sig_ s opq
big'_ x = Element "big" dats x

big__ :: AFSig__ s opq
big__ x = (\_ -> Element "big" dats [txt x])

big'__ :: Sig__ s opq
big'__ x = Element "big" dats [txt x]


blockquote :: AFSig s opq
blockquote x y = (\_ -> Element "blockquote" x y)

blockquote' :: Sig s opq
blockquote' = Element "blockquote"

blockquote_ :: AFSig_ s opq
blockquote_ x = (\_ -> Element "blockquote" dats x)

blockquote'_ :: Sig_ s opq
blockquote'_ x = Element "blockquote" dats x

blockquote__ :: AFSig__ s opq
blockquote__ x = (\_ -> Element "blockquote" dats [txt x])

blockquote'__ :: Sig__ s opq
blockquote'__ x = Element "blockquote" dats [txt x]


body :: AFSig s opq
body x y = (\_ -> Element "body" x y)

body' :: Sig s opq
body' = Element "body"

body_ :: AFSig_ s opq
body_ x = (\_ -> Element "body" dats x)

body'_ :: Sig_ s opq
body'_ x = Element "body" dats x

body__ :: AFSig__ s opq
body__ x = (\_ -> Element "body" dats [txt x])

body'__ :: Sig__ s opq
body'__ x = Element "body" dats [txt x]


br :: (s -> Node s opq)
br = (\_ -> Element "br" dats [])

button :: AFSig s opq
button x y = (\_ -> Element "button" x y)

button' :: Sig s opq
button' = Element "button"

button_ :: AFSig_ s opq
button_ x = (\_ -> Element "button" dats x)

button'_ :: Sig_ s opq
button'_ x = Element "button" dats x

button__ :: AFSig__ s opq
button__ x = (\_ -> Element "button" dats [txt x])

button'__ :: Sig__ s opq
button'__ x = Element "button" dats [txt x]


canvas :: AFSig s opq
canvas x y = (\_ -> Element "canvas" x y)

canvas' :: Sig s opq
canvas' = Element "canvas"

canvas_ :: AFSig_ s opq
canvas_ x = (\_ -> Element "canvas" dats x)

canvas'_ :: Sig_ s opq
canvas'_ x = Element "canvas" dats x

canvas__ :: AFSig__ s opq
canvas__ x = (\_ -> Element "canvas" dats [txt x])

canvas'__ :: Sig__ s opq
canvas'__ x = Element "canvas" dats [txt x]


caption :: AFSig s opq
caption x y = (\_ -> Element "caption" x y)

caption' :: Sig s opq
caption' = Element "caption"

caption_ :: AFSig_ s opq
caption_ x = (\_ -> Element "caption" dats x)

caption'_ :: Sig_ s opq
caption'_ x = Element "caption" dats x

caption__ :: AFSig__ s opq
caption__ x = (\_ -> Element "caption" dats [txt x])

caption'__ :: Sig__ s opq
caption'__ x = Element "caption" dats [txt x]


center :: AFSig s opq
center x y = (\_ -> Element "center" x y)

center' :: Sig s opq
center' = Element "center"

center_ :: AFSig_ s opq
center_ x = (\_ -> Element "center" dats x)

center'_ :: Sig_ s opq
center'_ x = Element "center" dats x

center__ :: AFSig__ s opq
center__ x = (\_ -> Element "center" dats [txt x])

center'__ :: Sig__ s opq
center'__ x = Element "center" dats [txt x]


cite :: AFSig s opq
cite x y = (\_ -> Element "cite" x y)

cite' :: Sig s opq
cite' = Element "cite"

cite_ :: AFSig_ s opq
cite_ x = (\_ -> Element "cite" dats x)

cite'_ :: Sig_ s opq
cite'_ x = Element "cite" dats x

cite__ :: AFSig__ s opq
cite__ x = (\_ -> Element "cite" dats [txt x])

cite'__ :: Sig__ s opq
cite'__ x = Element "cite" dats [txt x]


code :: AFSig s opq
code x y = (\_ -> Element "code" x y)

code' :: Sig s opq
code' = Element "code"

code_ :: AFSig_ s opq
code_ x = (\_ -> Element "code" dats x)

code'_ :: Sig_ s opq
code'_ x = Element "code" dats x

code__ :: AFSig__ s opq
code__ x = (\_ -> Element "code" dats [txt x])

code'__ :: Sig__ s opq
code'__ x = Element "code" dats [txt x]


col :: AFSig s opq
col x y = (\_ -> Element "col" x y)

col' :: Sig s opq
col' = Element "col"

col_ :: AFSig_ s opq
col_ x = (\_ -> Element "col" dats x)

col'_ :: Sig_ s opq
col'_ x = Element "col" dats x

col__ :: AFSig__ s opq
col__ x = (\_ -> Element "col" dats [txt x])

col'__ :: Sig__ s opq
col'__ x = Element "col" dats [txt x]


colgroup :: AFSig s opq
colgroup x y = (\_ -> Element "colgroup" x y)

colgroup' :: Sig s opq
colgroup' = Element "colgroup"

colgroup_ :: AFSig_ s opq
colgroup_ x = (\_ -> Element "colgroup" dats x)

colgroup'_ :: Sig_ s opq
colgroup'_ x = Element "colgroup" dats x

colgroup__ :: AFSig__ s opq
colgroup__ x = (\_ -> Element "colgroup" dats [txt x])

colgroup'__ :: Sig__ s opq
colgroup'__ x = Element "colgroup" dats [txt x]


_data :: AFSig s opq
_data x y = (\_ -> Element "_data" x y)

_data' :: Sig s opq
_data' = Element "_data"

_data_ :: AFSig_ s opq
_data_ x = (\_ -> Element "_data" dats x)

_data'_ :: Sig_ s opq
_data'_ x = Element "_data" dats x

_data__ :: AFSig__ s opq
_data__ x = (\_ -> Element "_data" dats [txt x])

_data'__ :: Sig__ s opq
_data'__ x = Element "_data" dats [txt x]


_datalist :: AFSig s opq
_datalist x y = (\_ -> Element "_datalist" x y)

_datalist' :: Sig s opq
_datalist' = Element "_datalist"

_datalist_ :: AFSig_ s opq
_datalist_ x = (\_ -> Element "_datalist" dats x)

_datalist'_ :: Sig_ s opq
_datalist'_ x = Element "_datalist" dats x

_datalist__ :: AFSig__ s opq
_datalist__ x = (\_ -> Element "_datalist" dats [txt x])

_datalist'__ :: Sig__ s opq
_datalist'__ x = Element "_datalist" dats [txt x]


dd :: AFSig s opq
dd x y = (\_ -> Element "dd" x y)

dd' :: Sig s opq
dd' = Element "dd"

dd_ :: AFSig_ s opq
dd_ x = (\_ -> Element "dd" dats x)

dd'_ :: Sig_ s opq
dd'_ x = Element "dd" dats x

dd__ :: AFSig__ s opq
dd__ x = (\_ -> Element "dd" dats [txt x])

dd'__ :: Sig__ s opq
dd'__ x = Element "dd" dats [txt x]


del :: AFSig s opq
del x y = (\_ -> Element "del" x y)

del' :: Sig s opq
del' = Element "del"

del_ :: AFSig_ s opq
del_ x = (\_ -> Element "del" dats x)

del'_ :: Sig_ s opq
del'_ x = Element "del" dats x

del__ :: AFSig__ s opq
del__ x = (\_ -> Element "del" dats [txt x])

del'__ :: Sig__ s opq
del'__ x = Element "del" dats [txt x]


details :: AFSig s opq
details x y = (\_ -> Element "details" x y)

details' :: Sig s opq
details' = Element "details"

details_ :: AFSig_ s opq
details_ x = (\_ -> Element "details" dats x)

details'_ :: Sig_ s opq
details'_ x = Element "details" dats x

details__ :: AFSig__ s opq
details__ x = (\_ -> Element "details" dats [txt x])

details'__ :: Sig__ s opq
details'__ x = Element "details" dats [txt x]


dfn :: AFSig s opq
dfn x y = (\_ -> Element "dfn" x y)

dfn' :: Sig s opq
dfn' = Element "dfn"

dfn_ :: AFSig_ s opq
dfn_ x = (\_ -> Element "dfn" dats x)

dfn'_ :: Sig_ s opq
dfn'_ x = Element "dfn" dats x

dfn__ :: AFSig__ s opq
dfn__ x = (\_ -> Element "dfn" dats [txt x])

dfn'__ :: Sig__ s opq
dfn'__ x = Element "dfn" dats [txt x]


dialog :: AFSig s opq
dialog x y = (\_ -> Element "dialog" x y)

dialog' :: Sig s opq
dialog' = Element "dialog"

dialog_ :: AFSig_ s opq
dialog_ x = (\_ -> Element "dialog" dats x)

dialog'_ :: Sig_ s opq
dialog'_ x = Element "dialog" dats x

dialog__ :: AFSig__ s opq
dialog__ x = (\_ -> Element "dialog" dats [txt x])

dialog'__ :: Sig__ s opq
dialog'__ x = Element "dialog" dats [txt x]


dir :: AFSig s opq
dir x y = (\_ -> Element "dir" x y)

dir' :: Sig s opq
dir' = Element "dir"

dir_ :: AFSig_ s opq
dir_ x = (\_ -> Element "dir" dats x)

dir'_ :: Sig_ s opq
dir'_ x = Element "dir" dats x

dir__ :: AFSig__ s opq
dir__ x = (\_ -> Element "dir" dats [txt x])

dir'__ :: Sig__ s opq
dir'__ x = Element "dir" dats [txt x]


div :: AFSig s opq
div x y = (\_ -> Element "div" x y)

div' :: Sig s opq
div' = Element "div"

div_ :: AFSig_ s opq
div_ x = (\_ -> Element "div" dats x)

div'_ :: Sig_ s opq
div'_ x = Element "div" dats x

div__ :: AFSig__ s opq
div__ x = (\_ -> Element "div" dats [txt x])

div'__ :: Sig__ s opq
div'__ x = Element "div" dats [txt x]


dl :: AFSig s opq
dl x y = (\_ -> Element "dl" x y)

dl' :: Sig s opq
dl' = Element "dl"

dl_ :: AFSig_ s opq
dl_ x = (\_ -> Element "dl" dats x)

dl'_ :: Sig_ s opq
dl'_ x = Element "dl" dats x

dl__ :: AFSig__ s opq
dl__ x = (\_ -> Element "dl" dats [txt x])

dl'__ :: Sig__ s opq
dl'__ x = Element "dl" dats [txt x]


dt :: AFSig s opq
dt x y = (\_ -> Element "dt" x y)

dt' :: Sig s opq
dt' = Element "dt"

dt_ :: AFSig_ s opq
dt_ x = (\_ -> Element "dt" dats x)

dt'_ :: Sig_ s opq
dt'_ x = Element "dt" dats x

dt__ :: AFSig__ s opq
dt__ x = (\_ -> Element "dt" dats [txt x])

dt'__ :: Sig__ s opq
dt'__ x = Element "dt" dats [txt x]


em :: AFSig s opq
em x y = (\_ -> Element "em" x y)

em' :: Sig s opq
em' = Element "em"

em_ :: AFSig_ s opq
em_ x = (\_ -> Element "em" dats x)

em'_ :: Sig_ s opq
em'_ x = Element "em" dats x

em__ :: AFSig__ s opq
em__ x = (\_ -> Element "em" dats [txt x])

em'__ :: Sig__ s opq
em'__ x = Element "em" dats [txt x]


embed :: AFSig s opq
embed x y = (\_ -> Element "embed" x y)

embed' :: Sig s opq
embed' = Element "embed"

embed_ :: AFSig_ s opq
embed_ x = (\_ -> Element "embed" dats x)

embed'_ :: Sig_ s opq
embed'_ x = Element "embed" dats x

embed__ :: AFSig__ s opq
embed__ x = (\_ -> Element "embed" dats [txt x])

embed'__ :: Sig__ s opq
embed'__ x = Element "embed" dats [txt x]


fieldset :: AFSig s opq
fieldset x y = (\_ -> Element "fieldset" x y)

fieldset' :: Sig s opq
fieldset' = Element "fieldset"

fieldset_ :: AFSig_ s opq
fieldset_ x = (\_ -> Element "fieldset" dats x)

fieldset'_ :: Sig_ s opq
fieldset'_ x = Element "fieldset" dats x

fieldset__ :: AFSig__ s opq
fieldset__ x = (\_ -> Element "fieldset" dats [txt x])

fieldset'__ :: Sig__ s opq
fieldset'__ x = Element "fieldset" dats [txt x]


figcaption :: AFSig s opq
figcaption x y = (\_ -> Element "figcaption" x y)

figcaption' :: Sig s opq
figcaption' = Element "figcaption"

figcaption_ :: AFSig_ s opq
figcaption_ x = (\_ -> Element "figcaption" dats x)

figcaption'_ :: Sig_ s opq
figcaption'_ x = Element "figcaption" dats x

figcaption__ :: AFSig__ s opq
figcaption__ x = (\_ -> Element "figcaption" dats [txt x])

figcaption'__ :: Sig__ s opq
figcaption'__ x = Element "figcaption" dats [txt x]


figure :: AFSig s opq
figure x y = (\_ -> Element "figure" x y)

figure' :: Sig s opq
figure' = Element "figure"

figure_ :: AFSig_ s opq
figure_ x = (\_ -> Element "figure" dats x)

figure'_ :: Sig_ s opq
figure'_ x = Element "figure" dats x

figure__ :: AFSig__ s opq
figure__ x = (\_ -> Element "figure" dats [txt x])

figure'__ :: Sig__ s opq
figure'__ x = Element "figure" dats [txt x]


font :: AFSig s opq
font x y = (\_ -> Element "font" x y)

font' :: Sig s opq
font' = Element "font"

font_ :: AFSig_ s opq
font_ x = (\_ -> Element "font" dats x)

font'_ :: Sig_ s opq
font'_ x = Element "font" dats x

font__ :: AFSig__ s opq
font__ x = (\_ -> Element "font" dats [txt x])

font'__ :: Sig__ s opq
font'__ x = Element "font" dats [txt x]


footer :: AFSig s opq
footer x y = (\_ -> Element "footer" x y)

footer' :: Sig s opq
footer' = Element "footer"

footer_ :: AFSig_ s opq
footer_ x = (\_ -> Element "footer" dats x)

footer'_ :: Sig_ s opq
footer'_ x = Element "footer" dats x

footer__ :: AFSig__ s opq
footer__ x = (\_ -> Element "footer" dats [txt x])

footer'__ :: Sig__ s opq
footer'__ x = Element "footer" dats [txt x]


form :: AFSig s opq
form x y = (\_ -> Element "form" x y)

form' :: Sig s opq
form' = Element "form"

form_ :: AFSig_ s opq
form_ x = (\_ -> Element "form" dats x)

form'_ :: Sig_ s opq
form'_ x = Element "form" dats x

form__ :: AFSig__ s opq
form__ x = (\_ -> Element "form" dats [txt x])

form'__ :: Sig__ s opq
form'__ x = Element "form" dats [txt x]


frame :: AFSig s opq
frame x y = (\_ -> Element "frame" x y)

frame' :: Sig s opq
frame' = Element "frame"

frame_ :: AFSig_ s opq
frame_ x = (\_ -> Element "frame" dats x)

frame'_ :: Sig_ s opq
frame'_ x = Element "frame" dats x

frame__ :: AFSig__ s opq
frame__ x = (\_ -> Element "frame" dats [txt x])

frame'__ :: Sig__ s opq
frame'__ x = Element "frame" dats [txt x]


frameset :: AFSig s opq
frameset x y = (\_ -> Element "frameset" x y)

frameset' :: Sig s opq
frameset' = Element "frameset"

frameset_ :: AFSig_ s opq
frameset_ x = (\_ -> Element "frameset" dats x)

frameset'_ :: Sig_ s opq
frameset'_ x = Element "frameset" dats x

frameset__ :: AFSig__ s opq
frameset__ x = (\_ -> Element "frameset" dats [txt x])

frameset'__ :: Sig__ s opq
frameset'__ x = Element "frameset" dats [txt x]

head :: AFSig s opq
head x y = (\_ -> Element "head" x y)

head' :: Sig s opq
head' = Element "head"

head_ :: AFSig_ s opq
head_ x = (\_ -> Element "head" dats x)

head'_ :: Sig_ s opq
head'_ x = Element "head" dats x

head__ :: AFSig__ s opq
head__ x = (\_ -> Element "head" dats [txt x])

head'__ :: Sig__ s opq
head'__ x = Element "head" dats [txt x]


header :: AFSig s opq
header x y = (\_ -> Element "header" x y)

header' :: Sig s opq
header' = Element "header"

header_ :: AFSig_ s opq
header_ x = (\_ -> Element "header" dats x)

header'_ :: Sig_ s opq
header'_ x = Element "header" dats x

header__ :: AFSig__ s opq
header__ x = (\_ -> Element "header" dats [txt x])

header'__ :: Sig__ s opq
header'__ x = Element "header" dats [txt x]


hr :: (s -> Node s opq)
hr = (\_ -> Element "br" dats [])

html :: AFSig s opq
html x y = (\_ -> Element "html" x y)

html' :: Sig s opq
html' = Element "html"

html_ :: AFSig_ s opq
html_ x = (\_ -> Element "html" dats x)

html'_ :: Sig_ s opq
html'_ x = Element "html" dats x

html__ :: AFSig__ s opq
html__ x = (\_ -> Element "html" dats [txt x])

html'__ :: Sig__ s opq
html'__ x = Element "html" dats [txt x]


i :: AFSig s opq
i x y = (\_ -> Element "i" x y)

i' :: Sig s opq
i' = Element "i"

i_ :: AFSig_ s opq
i_ x = (\_ -> Element "i" dats x)

i'_ :: Sig_ s opq
i'_ x = Element "i" dats x

i__ :: AFSig__ s opq
i__ x = (\_ -> Element "i" dats [txt x])

i'__ :: Sig__ s opq
i'__ x = Element "i" dats [txt x]


iframe :: AFSig s opq
iframe x y = (\_ -> Element "iframe" x y)

iframe' :: Sig s opq
iframe' = Element "iframe"

iframe_ :: AFSig_ s opq
iframe_ x = (\_ -> Element "iframe" dats x)

iframe'_ :: Sig_ s opq
iframe'_ x = Element "iframe" dats x

iframe__ :: AFSig__ s opq
iframe__ x = (\_ -> Element "iframe" dats [txt x])

iframe'__ :: Sig__ s opq
iframe'__ x = Element "iframe" dats [txt x]


img :: (s -> Attributes s opq) -> (s -> Node s opq)
img x = (\_ -> Element "img" x [])

img' :: (s -> Attributes s opq) -> Node s opq
img' x = Element "img" x []

img_ :: (s -> Node s opq)
img_ = (\_ -> Element "img" dats [])

img'_ :: Node s opq
img'_ = Element "img" dats []

input :: AFSig s opq
input x y = (\_ -> Element "input" x y)

input' :: Sig s opq
input' = Element "input"

input_ :: AFSig_ s opq
input_ x = (\_ -> Element "input" dats x)

input'_ :: Sig_ s opq
input'_ x = Element "input" dats x

input__ :: AFSig__ s opq
input__ x = (\_ -> Element "input" dats [txt x])

input'__ :: Sig__ s opq
input'__ x = Element "input" dats [txt x]


ins :: AFSig s opq
ins x y = (\_ -> Element "ins" x y)

ins' :: Sig s opq
ins' = Element "ins"

ins_ :: AFSig_ s opq
ins_ x = (\_ -> Element "ins" dats x)

ins'_ :: Sig_ s opq
ins'_ x = Element "ins" dats x

ins__ :: AFSig__ s opq
ins__ x = (\_ -> Element "ins" dats [txt x])

ins'__ :: Sig__ s opq
ins'__ x = Element "ins" dats [txt x]


kbd :: AFSig s opq
kbd x y = (\_ -> Element "kbd" x y)

kbd' :: Sig s opq
kbd' = Element "kbd"

kbd_ :: AFSig_ s opq
kbd_ x = (\_ -> Element "kbd" dats x)

kbd'_ :: Sig_ s opq
kbd'_ x = Element "kbd" dats x

kbd__ :: AFSig__ s opq
kbd__ x = (\_ -> Element "kbd" dats [txt x])

kbd'__ :: Sig__ s opq
kbd'__ x = Element "kbd" dats [txt x]


label :: AFSig s opq
label x y = (\_ -> Element "label" x y)

label' :: Sig s opq
label' = Element "label"

label_ :: AFSig_ s opq
label_ x = (\_ -> Element "label" dats x)

label'_ :: Sig_ s opq
label'_ x = Element "label" dats x

label__ :: AFSig__ s opq
label__ x = (\_ -> Element "label" dats [txt x])

label'__ :: Sig__ s opq
label'__ x = Element "label" dats [txt x]


legend :: AFSig s opq
legend x y = (\_ -> Element "legend" x y)

legend' :: Sig s opq
legend' = Element "legend"

legend_ :: AFSig_ s opq
legend_ x = (\_ -> Element "legend" dats x)

legend'_ :: Sig_ s opq
legend'_ x = Element "legend" dats x

legend__ :: AFSig__ s opq
legend__ x = (\_ -> Element "legend" dats [txt x])

legend'__ :: Sig__ s opq
legend'__ x = Element "legend" dats [txt x]


li :: AFSig s opq
li x y = (\_ -> Element "li" x y)

li' :: Sig s opq
li' = Element "li"

li_ :: AFSig_ s opq
li_ x = (\_ -> Element "li" dats x)

li'_ :: Sig_ s opq
li'_ x = Element "li" dats x

li__ :: AFSig__ s opq
li__ x = (\_ -> Element "li" dats [txt x])

li'__ :: Sig__ s opq
li'__ x = Element "li" dats [txt x]


link :: AFSig s opq
link x y = (\_ -> Element "link" x y)

link' :: Sig s opq
link' = Element "link"

link_ :: AFSig_ s opq
link_ x = (\_ -> Element "link" dats x)

link'_ :: Sig_ s opq
link'_ x = Element "link" dats x

link__ :: AFSig__ s opq
link__ x = (\_ -> Element "link" dats [txt x])

link'__ :: Sig__ s opq
link'__ x = Element "link" dats [txt x]


main :: AFSig s opq
main x y = (\_ -> Element "main" x y)

main' :: Sig s opq
main' = Element "main"

main_ :: AFSig_ s opq
main_ x = (\_ -> Element "main" dats x)

main'_ :: Sig_ s opq
main'_ x = Element "main" dats x

main__ :: AFSig__ s opq
main__ x = (\_ -> Element "main" dats [txt x])

main'__ :: Sig__ s opq
main'__ x = Element "main" dats [txt x]


map :: AFSig s opq
map x y = (\_ -> Element "map" x y)

map' :: Sig s opq
map' = Element "map"

map_ :: AFSig_ s opq
map_ x = (\_ -> Element "map" dats x)

map'_ :: Sig_ s opq
map'_ x = Element "map" dats x

map__ :: AFSig__ s opq
map__ x = (\_ -> Element "map" dats [txt x])

map'__ :: Sig__ s opq
map'__ x = Element "map" dats [txt x]


mark :: AFSig s opq
mark x y = (\_ -> Element "mark" x y)

mark' :: Sig s opq
mark' = Element "mark"

mark_ :: AFSig_ s opq
mark_ x = (\_ -> Element "mark" dats x)

mark'_ :: Sig_ s opq
mark'_ x = Element "mark" dats x

mark__ :: AFSig__ s opq
mark__ x = (\_ -> Element "mark" dats [txt x])

mark'__ :: Sig__ s opq
mark'__ x = Element "mark" dats [txt x]


meta :: AFSig s opq
meta x y = (\_ -> Element "meta" x y)

meta' :: Sig s opq
meta' = Element "meta"

meta_ :: AFSig_ s opq
meta_ x = (\_ -> Element "meta" dats x)

meta'_ :: Sig_ s opq
meta'_ x = Element "meta" dats x

meta__ :: AFSig__ s opq
meta__ x = (\_ -> Element "meta" dats [txt x])

meta'__ :: Sig__ s opq
meta'__ x = Element "meta" dats [txt x]


meter :: AFSig s opq
meter x y = (\_ -> Element "meter" x y)

meter' :: Sig s opq
meter' = Element "meter"

meter_ :: AFSig_ s opq
meter_ x = (\_ -> Element "meter" dats x)

meter'_ :: Sig_ s opq
meter'_ x = Element "meter" dats x

meter__ :: AFSig__ s opq
meter__ x = (\_ -> Element "meter" dats [txt x])

meter'__ :: Sig__ s opq
meter'__ x = Element "meter" dats [txt x]


nav :: AFSig s opq
nav x y = (\_ -> Element "nav" x y)

nav' :: Sig s opq
nav' = Element "nav"

nav_ :: AFSig_ s opq
nav_ x = (\_ -> Element "nav" dats x)

nav'_ :: Sig_ s opq
nav'_ x = Element "nav" dats x

nav__ :: AFSig__ s opq
nav__ x = (\_ -> Element "nav" dats [txt x])

nav'__ :: Sig__ s opq
nav'__ x = Element "nav" dats [txt x]


noframes :: AFSig s opq
noframes x y = (\_ -> Element "noframes" x y)

noframes' :: Sig s opq
noframes' = Element "noframes"

noframes_ :: AFSig_ s opq
noframes_ x = (\_ -> Element "noframes" dats x)

noframes'_ :: Sig_ s opq
noframes'_ x = Element "noframes" dats x

noframes__ :: AFSig__ s opq
noframes__ x = (\_ -> Element "noframes" dats [txt x])

noframes'__ :: Sig__ s opq
noframes'__ x = Element "noframes" dats [txt x]


noscript :: AFSig s opq
noscript x y = (\_ -> Element "noscript" x y)

noscript' :: Sig s opq
noscript' = Element "noscript"

noscript_ :: AFSig_ s opq
noscript_ x = (\_ -> Element "noscript" dats x)

noscript'_ :: Sig_ s opq
noscript'_ x = Element "noscript" dats x

noscript__ :: AFSig__ s opq
noscript__ x = (\_ -> Element "noscript" dats [txt x])

noscript'__ :: Sig__ s opq
noscript'__ x = Element "noscript" dats [txt x]


object :: AFSig s opq
object x y = (\_ -> Element "object" x y)

object' :: Sig s opq
object' = Element "object"

object_ :: AFSig_ s opq
object_ x = (\_ -> Element "object" dats x)

object'_ :: Sig_ s opq
object'_ x = Element "object" dats x

object__ :: AFSig__ s opq
object__ x = (\_ -> Element "object" dats [txt x])

object'__ :: Sig__ s opq
object'__ x = Element "object" dats [txt x]


ol :: AFSig s opq
ol x y = (\_ -> Element "ol" x y)

ol' :: Sig s opq
ol' = Element "ol"

ol_ :: AFSig_ s opq
ol_ x = (\_ -> Element "ol" dats x)

ol'_ :: Sig_ s opq
ol'_ x = Element "ol" dats x

ol__ :: AFSig__ s opq
ol__ x = (\_ -> Element "ol" dats [txt x])

ol'__ :: Sig__ s opq
ol'__ x = Element "ol" dats [txt x]


optgroup :: AFSig s opq
optgroup x y = (\_ -> Element "optgroup" x y)

optgroup' :: Sig s opq
optgroup' = Element "optgroup"

optgroup_ :: AFSig_ s opq
optgroup_ x = (\_ -> Element "optgroup" dats x)

optgroup'_ :: Sig_ s opq
optgroup'_ x = Element "optgroup" dats x

optgroup__ :: AFSig__ s opq
optgroup__ x = (\_ -> Element "optgroup" dats [txt x])

optgroup'__ :: Sig__ s opq
optgroup'__ x = Element "optgroup" dats [txt x]


option :: AFSig s opq
option x y = (\_ -> Element "option" x y)

option' :: Sig s opq
option' = Element "option"

option_ :: AFSig_ s opq
option_ x = (\_ -> Element "option" dats x)

option'_ :: Sig_ s opq
option'_ x = Element "option" dats x

option__ :: AFSig__ s opq
option__ x = (\_ -> Element "option" dats [txt x])

option'__ :: Sig__ s opq
option'__ x = Element "option" dats [txt x]


output :: AFSig s opq
output x y = (\_ -> Element "output" x y)

output' :: Sig s opq
output' = Element "output"

output_ :: AFSig_ s opq
output_ x = (\_ -> Element "output" dats x)

output'_ :: Sig_ s opq
output'_ x = Element "output" dats x

output__ :: AFSig__ s opq
output__ x = (\_ -> Element "output" dats [txt x])

output'__ :: Sig__ s opq
output'__ x = Element "output" dats [txt x]


p :: AFSig s opq
p x y = (\_ -> Element "p" x y)

p' :: Sig s opq
p' = Element "p"

p_ :: AFSig_ s opq
p_ x = (\_ -> Element "p" dats x)

p'_ :: Sig_ s opq
p'_ x = Element "p" dats x

p__ :: AFSig__ s opq
p__ x = (\_ -> Element "p" dats [txt x])

p'__ :: Sig__ s opq
p'__ x = Element "p" dats [txt x]


param :: AFSig s opq
param x y = (\_ -> Element "param" x y)

param' :: Sig s opq
param' = Element "param"

param_ :: AFSig_ s opq
param_ x = (\_ -> Element "param" dats x)

param'_ :: Sig_ s opq
param'_ x = Element "param" dats x

param__ :: AFSig__ s opq
param__ x = (\_ -> Element "param" dats [txt x])

param'__ :: Sig__ s opq
param'__ x = Element "param" dats [txt x]


picture :: AFSig s opq
picture x y = (\_ -> Element "picture" x y)

picture' :: Sig s opq
picture' = Element "picture"

picture_ :: AFSig_ s opq
picture_ x = (\_ -> Element "picture" dats x)

picture'_ :: Sig_ s opq
picture'_ x = Element "picture" dats x

picture__ :: AFSig__ s opq
picture__ x = (\_ -> Element "picture" dats [txt x])

picture'__ :: Sig__ s opq
picture'__ x = Element "picture" dats [txt x]


pre :: AFSig s opq
pre x y = (\_ -> Element "pre" x y)

pre' :: Sig s opq
pre' = Element "pre"

pre_ :: AFSig_ s opq
pre_ x = (\_ -> Element "pre" dats x)

pre'_ :: Sig_ s opq
pre'_ x = Element "pre" dats x

pre__ :: AFSig__ s opq
pre__ x = (\_ -> Element "pre" dats [txt x])

pre'__ :: Sig__ s opq
pre'__ x = Element "pre" dats [txt x]


progress :: AFSig s opq
progress x y = (\_ -> Element "progress" x y)

progress' :: Sig s opq
progress' = Element "progress"

progress_ :: AFSig_ s opq
progress_ x = (\_ -> Element "progress" dats x)

progress'_ :: Sig_ s opq
progress'_ x = Element "progress" dats x

progress__ :: AFSig__ s opq
progress__ x = (\_ -> Element "progress" dats [txt x])

progress'__ :: Sig__ s opq
progress'__ x = Element "progress" dats [txt x]


q :: AFSig s opq
q x y = (\_ -> Element "q" x y)

q' :: Sig s opq
q' = Element "q"

q_ :: AFSig_ s opq
q_ x = (\_ -> Element "q" dats x)

q'_ :: Sig_ s opq
q'_ x = Element "q" dats x

q__ :: AFSig__ s opq
q__ x = (\_ -> Element "q" dats [txt x])

q'__ :: Sig__ s opq
q'__ x = Element "q" dats [txt x]


rp :: AFSig s opq
rp x y = (\_ -> Element "rp" x y)

rp' :: Sig s opq
rp' = Element "rp"

rp_ :: AFSig_ s opq
rp_ x = (\_ -> Element "rp" dats x)

rp'_ :: Sig_ s opq
rp'_ x = Element "rp" dats x

rp__ :: AFSig__ s opq
rp__ x = (\_ -> Element "rp" dats [txt x])

rp'__ :: Sig__ s opq
rp'__ x = Element "rp" dats [txt x]


rt :: AFSig s opq
rt x y = (\_ -> Element "rt" x y)

rt' :: Sig s opq
rt' = Element "rt"

rt_ :: AFSig_ s opq
rt_ x = (\_ -> Element "rt" dats x)

rt'_ :: Sig_ s opq
rt'_ x = Element "rt" dats x

rt__ :: AFSig__ s opq
rt__ x = (\_ -> Element "rt" dats [txt x])

rt'__ :: Sig__ s opq
rt'__ x = Element "rt" dats [txt x]


ruby :: AFSig s opq
ruby x y = (\_ -> Element "ruby" x y)

ruby' :: Sig s opq
ruby' = Element "ruby"

ruby_ :: AFSig_ s opq
ruby_ x = (\_ -> Element "ruby" dats x)

ruby'_ :: Sig_ s opq
ruby'_ x = Element "ruby" dats x

ruby__ :: AFSig__ s opq
ruby__ x = (\_ -> Element "ruby" dats [txt x])

ruby'__ :: Sig__ s opq
ruby'__ x = Element "ruby" dats [txt x]


s :: AFSig s opq
s x y = (\_ -> Element "s" x y)

s' :: Sig s opq
s' = Element "s"

s_ :: AFSig_ s opq
s_ x = (\_ -> Element "s" dats x)

s'_ :: Sig_ s opq
s'_ x = Element "s" dats x

s__ :: AFSig__ s opq
s__ x = (\_ -> Element "s" dats [txt x])

s'__ :: Sig__ s opq
s'__ x = Element "s" dats [txt x]


samp :: AFSig s opq
samp x y = (\_ -> Element "samp" x y)

samp' :: Sig s opq
samp' = Element "samp"

samp_ :: AFSig_ s opq
samp_ x = (\_ -> Element "samp" dats x)

samp'_ :: Sig_ s opq
samp'_ x = Element "samp" dats x

samp__ :: AFSig__ s opq
samp__ x = (\_ -> Element "samp" dats [txt x])

samp'__ :: Sig__ s opq
samp'__ x = Element "samp" dats [txt x]


script :: AFSig s opq
script x y = (\_ -> Element "script" x y)

script' :: Sig s opq
script' = Element "script"

script_ :: AFSig_ s opq
script_ x = (\_ -> Element "script" dats x)

script'_ :: Sig_ s opq
script'_ x = Element "script" dats x

script__ :: AFSig__ s opq
script__ x = (\_ -> Element "script" dats [txt x])

script'__ :: Sig__ s opq
script'__ x = Element "script" dats [txt x]


section :: AFSig s opq
section x y = (\_ -> Element "section" x y)

section' :: Sig s opq
section' = Element "section"

section_ :: AFSig_ s opq
section_ x = (\_ -> Element "section" dats x)

section'_ :: Sig_ s opq
section'_ x = Element "section" dats x

section__ :: AFSig__ s opq
section__ x = (\_ -> Element "section" dats [txt x])

section'__ :: Sig__ s opq
section'__ x = Element "section" dats [txt x]


select :: AFSig s opq
select x y = (\_ -> Element "select" x y)

select' :: Sig s opq
select' = Element "select"

select_ :: AFSig_ s opq
select_ x = (\_ -> Element "select" dats x)

select'_ :: Sig_ s opq
select'_ x = Element "select" dats x

select__ :: AFSig__ s opq
select__ x = (\_ -> Element "select" dats [txt x])

select'__ :: Sig__ s opq
select'__ x = Element "select" dats [txt x]


small :: AFSig s opq
small x y = (\_ -> Element "small" x y)

small' :: Sig s opq
small' = Element "small"

small_ :: AFSig_ s opq
small_ x = (\_ -> Element "small" dats x)

small'_ :: Sig_ s opq
small'_ x = Element "small" dats x

small__ :: AFSig__ s opq
small__ x = (\_ -> Element "small" dats [txt x])

small'__ :: Sig__ s opq
small'__ x = Element "small" dats [txt x]


source :: AFSig s opq
source x y = (\_ -> Element "source" x y)

source' :: Sig s opq
source' = Element "source"

source_ :: AFSig_ s opq
source_ x = (\_ -> Element "source" dats x)

source'_ :: Sig_ s opq
source'_ x = Element "source" dats x

source__ :: AFSig__ s opq
source__ x = (\_ -> Element "source" dats [txt x])

source'__ :: Sig__ s opq
source'__ x = Element "source" dats [txt x]


span :: AFSig s opq
span x y = (\_ -> Element "span" x y)

span' :: Sig s opq
span' = Element "span"

span_ :: AFSig_ s opq
span_ x = (\_ -> Element "span" dats x)

span'_ :: Sig_ s opq
span'_ x = Element "span" dats x

span__ :: AFSig__ s opq
span__ x = (\_ -> Element "span" dats [txt x])

span'__ :: Sig__ s opq
span'__ x = Element "span" dats [txt x]


strike :: AFSig s opq
strike x y = (\_ -> Element "strike" x y)

strike' :: Sig s opq
strike' = Element "strike"

strike_ :: AFSig_ s opq
strike_ x = (\_ -> Element "strike" dats x)

strike'_ :: Sig_ s opq
strike'_ x = Element "strike" dats x

strike__ :: AFSig__ s opq
strike__ x = (\_ -> Element "strike" dats [txt x])

strike'__ :: Sig__ s opq
strike'__ x = Element "strike" dats [txt x]


strong :: AFSig s opq
strong x y = (\_ -> Element "strong" x y)

strong' :: Sig s opq
strong' = Element "strong"

strong_ :: AFSig_ s opq
strong_ x = (\_ -> Element "strong" dats x)

strong'_ :: Sig_ s opq
strong'_ x = Element "strong" dats x

strong__ :: AFSig__ s opq
strong__ x = (\_ -> Element "strong" dats [txt x])

strong'__ :: Sig__ s opq
strong'__ x = Element "strong" dats [txt x]


style :: AFSig s opq
style x y = (\_ -> Element "style" x y)

style' :: Sig s opq
style' = Element "style"

style_ :: AFSig_ s opq
style_ x = (\_ -> Element "style" dats x)

style'_ :: Sig_ s opq
style'_ x = Element "style" dats x

style__ :: AFSig__ s opq
style__ x = (\_ -> Element "style" dats [txt x])

style'__ :: Sig__ s opq
style'__ x = Element "style" dats [txt x]


sub :: AFSig s opq
sub x y = (\_ -> Element "sub" x y)

sub' :: Sig s opq
sub' = Element "sub"

sub_ :: AFSig_ s opq
sub_ x = (\_ -> Element "sub" dats x)

sub'_ :: Sig_ s opq
sub'_ x = Element "sub" dats x

sub__ :: AFSig__ s opq
sub__ x = (\_ -> Element "sub" dats [txt x])

sub'__ :: Sig__ s opq
sub'__ x = Element "sub" dats [txt x]


summary :: AFSig s opq
summary x y = (\_ -> Element "summary" x y)

summary' :: Sig s opq
summary' = Element "summary"

summary_ :: AFSig_ s opq
summary_ x = (\_ -> Element "summary" dats x)

summary'_ :: Sig_ s opq
summary'_ x = Element "summary" dats x

summary__ :: AFSig__ s opq
summary__ x = (\_ -> Element "summary" dats [txt x])

summary'__ :: Sig__ s opq
summary'__ x = Element "summary" dats [txt x]


sup :: AFSig s opq
sup x y = (\_ -> Element "sup" x y)

sup' :: Sig s opq
sup' = Element "sup"

sup_ :: AFSig_ s opq
sup_ x = (\_ -> Element "sup" dats x)

sup'_ :: Sig_ s opq
sup'_ x = Element "sup" dats x

sup__ :: AFSig__ s opq
sup__ x = (\_ -> Element "sup" dats [txt x])

sup'__ :: Sig__ s opq
sup'__ x = Element "sup" dats [txt x]


svg :: AFSig s opq
svg x y = (\_ -> Element "svg" x y)

svg' :: Sig s opq
svg' = Element "svg"

svg_ :: AFSig_ s opq
svg_ x = (\_ -> Element "svg" dats x)

svg'_ :: Sig_ s opq
svg'_ x = Element "svg" dats x

svg__ :: AFSig__ s opq
svg__ x = (\_ -> Element "svg" dats [txt x])

svg'__ :: Sig__ s opq
svg'__ x = Element "svg" dats [txt x]


table :: AFSig s opq
table x y = (\_ -> Element "table" x y)

table' :: Sig s opq
table' = Element "table"

table_ :: AFSig_ s opq
table_ x = (\_ -> Element "table" dats x)

table'_ :: Sig_ s opq
table'_ x = Element "table" dats x

table__ :: AFSig__ s opq
table__ x = (\_ -> Element "table" dats [txt x])

table'__ :: Sig__ s opq
table'__ x = Element "table" dats [txt x]


tbody :: AFSig s opq
tbody x y = (\_ -> Element "tbody" x y)

tbody' :: Sig s opq
tbody' = Element "tbody"

tbody_ :: AFSig_ s opq
tbody_ x = (\_ -> Element "tbody" dats x)

tbody'_ :: Sig_ s opq
tbody'_ x = Element "tbody" dats x

tbody__ :: AFSig__ s opq
tbody__ x = (\_ -> Element "tbody" dats [txt x])

tbody'__ :: Sig__ s opq
tbody'__ x = Element "tbody" dats [txt x]


td :: AFSig s opq
td x y = (\_ -> Element "td" x y)

td' :: Sig s opq
td' = Element "td"

td_ :: AFSig_ s opq
td_ x = (\_ -> Element "td" dats x)

td'_ :: Sig_ s opq
td'_ x = Element "td" dats x

td__ :: AFSig__ s opq
td__ x = (\_ -> Element "td" dats [txt x])

td'__ :: Sig__ s opq
td'__ x = Element "td" dats [txt x]


template :: AFSig s opq
template x y = (\_ -> Element "template" x y)

template' :: Sig s opq
template' = Element "template"

template_ :: AFSig_ s opq
template_ x = (\_ -> Element "template" dats x)

template'_ :: Sig_ s opq
template'_ x = Element "template" dats x

template__ :: AFSig__ s opq
template__ x = (\_ -> Element "template" dats [txt x])

template'__ :: Sig__ s opq
template'__ x = Element "template" dats [txt x]


textarea :: AFSig s opq
textarea x y = (\_ -> Element "textarea" x y)

textarea' :: Sig s opq
textarea' = Element "textarea"

textarea_ :: AFSig_ s opq
textarea_ x = (\_ -> Element "textarea" dats x)

textarea'_ :: Sig_ s opq
textarea'_ x = Element "textarea" dats x

textarea__ :: AFSig__ s opq
textarea__ x = (\_ -> Element "textarea" dats [txt x])

textarea'__ :: Sig__ s opq
textarea'__ x = Element "textarea" dats [txt x]


tfoot :: AFSig s opq
tfoot x y = (\_ -> Element "tfoot" x y)

tfoot' :: Sig s opq
tfoot' = Element "tfoot"

tfoot_ :: AFSig_ s opq
tfoot_ x = (\_ -> Element "tfoot" dats x)

tfoot'_ :: Sig_ s opq
tfoot'_ x = Element "tfoot" dats x

tfoot__ :: AFSig__ s opq
tfoot__ x = (\_ -> Element "tfoot" dats [txt x])

tfoot'__ :: Sig__ s opq
tfoot'__ x = Element "tfoot" dats [txt x]


th :: AFSig s opq
th x y = (\_ -> Element "th" x y)

th' :: Sig s opq
th' = Element "th"

th_ :: AFSig_ s opq
th_ x = (\_ -> Element "th" dats x)

th'_ :: Sig_ s opq
th'_ x = Element "th" dats x

th__ :: AFSig__ s opq
th__ x = (\_ -> Element "th" dats [txt x])

th'__ :: Sig__ s opq
th'__ x = Element "th" dats [txt x]


thead :: AFSig s opq
thead x y = (\_ -> Element "thead" x y)

thead' :: Sig s opq
thead' = Element "thead"

thead_ :: AFSig_ s opq
thead_ x = (\_ -> Element "thead" dats x)

thead'_ :: Sig_ s opq
thead'_ x = Element "thead" dats x

thead__ :: AFSig__ s opq
thead__ x = (\_ -> Element "thead" dats [txt x])

thead'__ :: Sig__ s opq
thead'__ x = Element "thead" dats [txt x]


time :: AFSig s opq
time x y = (\_ -> Element "time" x y)

time' :: Sig s opq
time' = Element "time"

time_ :: AFSig_ s opq
time_ x = (\_ -> Element "time" dats x)

time'_ :: Sig_ s opq
time'_ x = Element "time" dats x

time__ :: AFSig__ s opq
time__ x = (\_ -> Element "time" dats [txt x])

time'__ :: Sig__ s opq
time'__ x = Element "time" dats [txt x]


title :: AFSig s opq
title x y = (\_ -> Element "title" x y)

title' :: Sig s opq
title' = Element "title"

title_ :: AFSig_ s opq
title_ x = (\_ -> Element "title" dats x)

title'_ :: Sig_ s opq
title'_ x = Element "title" dats x

title__ :: AFSig__ s opq
title__ x = (\_ -> Element "title" dats [txt x])

title'__ :: Sig__ s opq
title'__ x = Element "title" dats [txt x]


tr :: AFSig s opq
tr x y = (\_ -> Element "tr" x y)

tr' :: Sig s opq
tr' = Element "tr"

tr_ :: AFSig_ s opq
tr_ x = (\_ -> Element "tr" dats x)

tr'_ :: Sig_ s opq
tr'_ x = Element "tr" dats x

tr__ :: AFSig__ s opq
tr__ x = (\_ -> Element "tr" dats [txt x])

tr'__ :: Sig__ s opq
tr'__ x = Element "tr" dats [txt x]


track :: AFSig s opq
track x y = (\_ -> Element "track" x y)

track' :: Sig s opq
track' = Element "track"

track_ :: AFSig_ s opq
track_ x = (\_ -> Element "track" dats x)

track'_ :: Sig_ s opq
track'_ x = Element "track" dats x

track__ :: AFSig__ s opq
track__ x = (\_ -> Element "track" dats [txt x])

track'__ :: Sig__ s opq
track'__ x = Element "track" dats [txt x]


tt :: AFSig s opq
tt x y = (\_ -> Element "tt" x y)

tt' :: Sig s opq
tt' = Element "tt"

tt_ :: AFSig_ s opq
tt_ x = (\_ -> Element "tt" dats x)

tt'_ :: Sig_ s opq
tt'_ x = Element "tt" dats x

tt__ :: AFSig__ s opq
tt__ x = (\_ -> Element "tt" dats [txt x])

tt'__ :: Sig__ s opq
tt'__ x = Element "tt" dats [txt x]


u :: AFSig s opq
u x y = (\_ -> Element "u" x y)

u' :: Sig s opq
u' = Element "u"

u_ :: AFSig_ s opq
u_ x = (\_ -> Element "u" dats x)

u'_ :: Sig_ s opq
u'_ x = Element "u" dats x

u__ :: AFSig__ s opq
u__ x = (\_ -> Element "u" dats [txt x])

u'__ :: Sig__ s opq
u'__ x = Element "u" dats [txt x]


ul :: AFSig s opq
ul x y = (\_ -> Element "ul" x y)

ul' :: Sig s opq
ul' = Element "ul"

ul_ :: AFSig_ s opq
ul_ x = (\_ -> Element "ul" dats x)

ul'_ :: Sig_ s opq
ul'_ x = Element "ul" dats x

ul__ :: AFSig__ s opq
ul__ x = (\_ -> Element "ul" dats [txt x])

ul'__ :: Sig__ s opq
ul'__ x = Element "ul" dats [txt x]


var :: AFSig s opq
var x y = (\_ -> Element "var" x y)

var' :: Sig s opq
var' = Element "var"

var_ :: AFSig_ s opq
var_ x = (\_ -> Element "var" dats x)

var'_ :: Sig_ s opq
var'_ x = Element "var" dats x

var__ :: AFSig__ s opq
var__ x = (\_ -> Element "var" dats [txt x])

var'__ :: Sig__ s opq
var'__ x = Element "var" dats [txt x]


video :: AFSig s opq
video x y = (\_ -> Element "video" x y)

video' :: Sig s opq
video' = Element "video"

video_ :: AFSig_ s opq
video_ x = (\_ -> Element "video" dats x)

video'_ :: Sig_ s opq
video'_ x = Element "video" dats x

video__ :: AFSig__ s opq
video__ x = (\_ -> Element "video" dats [txt x])

video'__ :: Sig__ s opq
video'__ x = Element "video" dats [txt x]


wbr :: (s -> Node s opq)
wbr = (\_ -> Element "br" dats [])

txt :: String -> (s -> Node s opq)
txt t = (\_ -> TextNode t)

txt' :: String -> Node s opq
txt' = TextNode


h1 :: AFSig s opq
h1 x y = (\_ -> Element "h1" x y)

h1' :: Sig s opq
h1' = Element "h1"

h1_ :: AFSig_ s opq
h1_ x = (\_ -> Element "h1" dats x)

h1'_ :: Sig_ s opq
h1'_ x = Element "h1" dats x

h1__ :: AFSig__ s opq
h1__ x = (\_ -> Element "h1" dats [txt x])

h1'__ :: Sig__ s opq
h1'__ x = Element "h1" dats [txt x]


h2 :: AFSig s opq
h2 x y = (\_ -> Element "h2" x y)

h2' :: Sig s opq
h2' = Element "h2"

h2_ :: AFSig_ s opq
h2_ x = (\_ -> Element "h2" dats x)

h2'_ :: Sig_ s opq
h2'_ x = Element "h2" dats x

h2__ :: AFSig__ s opq
h2__ x = (\_ -> Element "h2" dats [txt x])

h2'__ :: Sig__ s opq
h2'__ x = Element "h2" dats [txt x]


h3 :: AFSig s opq
h3 x y = (\_ -> Element "h3" x y)

h3' :: Sig s opq
h3' = Element "h3"

h3_ :: AFSig_ s opq
h3_ x = (\_ -> Element "h3" dats x)

h3'_ :: Sig_ s opq
h3'_ x = Element "h3" dats x

h3__ :: AFSig__ s opq
h3__ x = (\_ -> Element "h3" dats [txt x])

h3'__ :: Sig__ s opq
h3'__ x = Element "h3" dats [txt x]


h4 :: AFSig s opq
h4 x y = (\_ -> Element "h4" x y)

h4' :: Sig s opq
h4' = Element "h4"

h4_ :: AFSig_ s opq
h4_ x = (\_ -> Element "h4" dats x)

h4'_ :: Sig_ s opq
h4'_ x = Element "h4" dats x

h4__ :: AFSig__ s opq
h4__ x = (\_ -> Element "h4" dats [txt x])

h4'__ :: Sig__ s opq
h4'__ x = Element "h4" dats [txt x]


h5 :: AFSig s opq
h5 x y = (\_ -> Element "h5" x y)

h5' :: Sig s opq
h5' = Element "h5"

h5_ :: AFSig_ s opq
h5_ x = (\_ -> Element "h5" dats x)

h5'_ :: Sig_ s opq
h5'_ x = Element "h5" dats x

h5__ :: AFSig__ s opq
h5__ x = (\_ -> Element "h5" dats [txt x])

h5'__ :: Sig__ s opq
h5'__ x = Element "h5" dats [txt x]


h6 :: AFSig s opq
h6 x y = (\_ -> Element "h6" x y)

h6' :: Sig s opq
h6' = Element "h6"

h6_ :: AFSig_ s opq
h6_ x = (\_ -> Element "h6" dats x)

h6'_ :: Sig_ s opq
h6'_ x = Element "h6" dats x

h6__ :: AFSig__ s opq
h6__ x = (\_ -> Element "h6" dats [txt x])

h6'__ :: Sig__ s opq
h6'__ x = Element "h6" dats [txt x]
