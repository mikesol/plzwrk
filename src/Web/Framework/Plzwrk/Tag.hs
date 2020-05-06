{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Framework.Plzwrk.Tag
Description : Useful tags like div and img
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
  = [(String, (s -> PwAttribute s opq))] -> [s -> PwNode s opq] -> (s -> PwNode s opq)
type Sig s opq = [(String, (s -> PwAttribute s opq))] -> [s -> PwNode s opq] -> PwNode s opq

type AFSig_ s opq = [s -> PwNode s opq] -> (s -> PwNode s opq)
type Sig_ s opq = [s -> PwNode s opq] -> PwNode s opq

type AFSig__ s opq = String -> (s -> PwNode s opq)
type Sig__ s opq = String -> PwNode s opq


a :: AFSig s opq
a x y = (\_ -> PwElement "a" x y)

a' :: Sig s opq
a' = PwElement "a"

a_ :: AFSig_ s opq
a_ x = (\_ -> PwElement "a" dats x)

a'_ :: Sig_ s opq
a'_ x = PwElement "a" dats x

a__ :: AFSig__ s opq
a__ x = (\_ -> PwElement "a" dats [txt x])

a'__ :: Sig__ s opq
a'__ x = PwElement "a" dats [txt x]


abbr :: AFSig s opq
abbr x y = (\_ -> PwElement "abbr" x y)

abbr' :: Sig s opq
abbr' = PwElement "abbr"

abbr_ :: AFSig_ s opq
abbr_ x = (\_ -> PwElement "abbr" dats x)

abbr'_ :: Sig_ s opq
abbr'_ x = PwElement "abbr" dats x

abbr__ :: AFSig__ s opq
abbr__ x = (\_ -> PwElement "abbr" dats [txt x])

abbr'__ :: Sig__ s opq
abbr'__ x = PwElement "abbr" dats [txt x]


acronym :: AFSig s opq
acronym x y = (\_ -> PwElement "acronym" x y)

acronym' :: Sig s opq
acronym' = PwElement "acronym"

acronym_ :: AFSig_ s opq
acronym_ x = (\_ -> PwElement "acronym" dats x)

acronym'_ :: Sig_ s opq
acronym'_ x = PwElement "acronym" dats x

acronym__ :: AFSig__ s opq
acronym__ x = (\_ -> PwElement "acronym" dats [txt x])

acronym'__ :: Sig__ s opq
acronym'__ x = PwElement "acronym" dats [txt x]


address :: AFSig s opq
address x y = (\_ -> PwElement "address" x y)

address' :: Sig s opq
address' = PwElement "address"

address_ :: AFSig_ s opq
address_ x = (\_ -> PwElement "address" dats x)

address'_ :: Sig_ s opq
address'_ x = PwElement "address" dats x

address__ :: AFSig__ s opq
address__ x = (\_ -> PwElement "address" dats [txt x])

address'__ :: Sig__ s opq
address'__ x = PwElement "address" dats [txt x]


applet :: AFSig s opq
applet x y = (\_ -> PwElement "applet" x y)

applet' :: Sig s opq
applet' = PwElement "applet"

applet_ :: AFSig_ s opq
applet_ x = (\_ -> PwElement "applet" dats x)

applet'_ :: Sig_ s opq
applet'_ x = PwElement "applet" dats x

applet__ :: AFSig__ s opq
applet__ x = (\_ -> PwElement "applet" dats [txt x])

applet'__ :: Sig__ s opq
applet'__ x = PwElement "applet" dats [txt x]


area :: AFSig s opq
area x y = (\_ -> PwElement "area" x y)

area' :: Sig s opq
area' = PwElement "area"

area_ :: AFSig_ s opq
area_ x = (\_ -> PwElement "area" dats x)

area'_ :: Sig_ s opq
area'_ x = PwElement "area" dats x

area__ :: AFSig__ s opq
area__ x = (\_ -> PwElement "area" dats [txt x])

area'__ :: Sig__ s opq
area'__ x = PwElement "area" dats [txt x]


article :: AFSig s opq
article x y = (\_ -> PwElement "article" x y)

article' :: Sig s opq
article' = PwElement "article"

article_ :: AFSig_ s opq
article_ x = (\_ -> PwElement "article" dats x)

article'_ :: Sig_ s opq
article'_ x = PwElement "article" dats x

article__ :: AFSig__ s opq
article__ x = (\_ -> PwElement "article" dats [txt x])

article'__ :: Sig__ s opq
article'__ x = PwElement "article" dats [txt x]


aside :: AFSig s opq
aside x y = (\_ -> PwElement "aside" x y)

aside' :: Sig s opq
aside' = PwElement "aside"

aside_ :: AFSig_ s opq
aside_ x = (\_ -> PwElement "aside" dats x)

aside'_ :: Sig_ s opq
aside'_ x = PwElement "aside" dats x

aside__ :: AFSig__ s opq
aside__ x = (\_ -> PwElement "aside" dats [txt x])

aside'__ :: Sig__ s opq
aside'__ x = PwElement "aside" dats [txt x]


audio :: AFSig s opq
audio x y = (\_ -> PwElement "audio" x y)

audio' :: Sig s opq
audio' = PwElement "audio"

audio_ :: AFSig_ s opq
audio_ x = (\_ -> PwElement "audio" dats x)

audio'_ :: Sig_ s opq
audio'_ x = PwElement "audio" dats x

audio__ :: AFSig__ s opq
audio__ x = (\_ -> PwElement "audio" dats [txt x])

audio'__ :: Sig__ s opq
audio'__ x = PwElement "audio" dats [txt x]


b :: AFSig s opq
b x y = (\_ -> PwElement "b" x y)

b' :: Sig s opq
b' = PwElement "b"

b_ :: AFSig_ s opq
b_ x = (\_ -> PwElement "b" dats x)

b'_ :: Sig_ s opq
b'_ x = PwElement "b" dats x

b__ :: AFSig__ s opq
b__ x = (\_ -> PwElement "b" dats [txt x])

b'__ :: Sig__ s opq
b'__ x = PwElement "b" dats [txt x]


base :: AFSig s opq
base x y = (\_ -> PwElement "base" x y)

base' :: Sig s opq
base' = PwElement "base"

base_ :: AFSig_ s opq
base_ x = (\_ -> PwElement "base" dats x)

base'_ :: Sig_ s opq
base'_ x = PwElement "base" dats x

base__ :: AFSig__ s opq
base__ x = (\_ -> PwElement "base" dats [txt x])

base'__ :: Sig__ s opq
base'__ x = PwElement "base" dats [txt x]


basefont :: AFSig s opq
basefont x y = (\_ -> PwElement "basefont" x y)

basefont' :: Sig s opq
basefont' = PwElement "basefont"

basefont_ :: AFSig_ s opq
basefont_ x = (\_ -> PwElement "basefont" dats x)

basefont'_ :: Sig_ s opq
basefont'_ x = PwElement "basefont" dats x

basefont__ :: AFSig__ s opq
basefont__ x = (\_ -> PwElement "basefont" dats [txt x])

basefont'__ :: Sig__ s opq
basefont'__ x = PwElement "basefont" dats [txt x]


bdi :: AFSig s opq
bdi x y = (\_ -> PwElement "bdi" x y)

bdi' :: Sig s opq
bdi' = PwElement "bdi"

bdi_ :: AFSig_ s opq
bdi_ x = (\_ -> PwElement "bdi" dats x)

bdi'_ :: Sig_ s opq
bdi'_ x = PwElement "bdi" dats x

bdi__ :: AFSig__ s opq
bdi__ x = (\_ -> PwElement "bdi" dats [txt x])

bdi'__ :: Sig__ s opq
bdi'__ x = PwElement "bdi" dats [txt x]


bdo :: AFSig s opq
bdo x y = (\_ -> PwElement "bdo" x y)

bdo' :: Sig s opq
bdo' = PwElement "bdo"

bdo_ :: AFSig_ s opq
bdo_ x = (\_ -> PwElement "bdo" dats x)

bdo'_ :: Sig_ s opq
bdo'_ x = PwElement "bdo" dats x

bdo__ :: AFSig__ s opq
bdo__ x = (\_ -> PwElement "bdo" dats [txt x])

bdo'__ :: Sig__ s opq
bdo'__ x = PwElement "bdo" dats [txt x]


big :: AFSig s opq
big x y = (\_ -> PwElement "big" x y)

big' :: Sig s opq
big' = PwElement "big"

big_ :: AFSig_ s opq
big_ x = (\_ -> PwElement "big" dats x)

big'_ :: Sig_ s opq
big'_ x = PwElement "big" dats x

big__ :: AFSig__ s opq
big__ x = (\_ -> PwElement "big" dats [txt x])

big'__ :: Sig__ s opq
big'__ x = PwElement "big" dats [txt x]


blockquote :: AFSig s opq
blockquote x y = (\_ -> PwElement "blockquote" x y)

blockquote' :: Sig s opq
blockquote' = PwElement "blockquote"

blockquote_ :: AFSig_ s opq
blockquote_ x = (\_ -> PwElement "blockquote" dats x)

blockquote'_ :: Sig_ s opq
blockquote'_ x = PwElement "blockquote" dats x

blockquote__ :: AFSig__ s opq
blockquote__ x = (\_ -> PwElement "blockquote" dats [txt x])

blockquote'__ :: Sig__ s opq
blockquote'__ x = PwElement "blockquote" dats [txt x]


body :: AFSig s opq
body x y = (\_ -> PwElement "body" x y)

body' :: Sig s opq
body' = PwElement "body"

body_ :: AFSig_ s opq
body_ x = (\_ -> PwElement "body" dats x)

body'_ :: Sig_ s opq
body'_ x = PwElement "body" dats x

body__ :: AFSig__ s opq
body__ x = (\_ -> PwElement "body" dats [txt x])

body'__ :: Sig__ s opq
body'__ x = PwElement "body" dats [txt x]


br :: (s -> PwNode s opq)
br = (\_ -> PwElement "br" dats [])

button :: AFSig s opq
button x y = (\_ -> PwElement "button" x y)

button' :: Sig s opq
button' = PwElement "button"

button_ :: AFSig_ s opq
button_ x = (\_ -> PwElement "button" dats x)

button'_ :: Sig_ s opq
button'_ x = PwElement "button" dats x

button__ :: AFSig__ s opq
button__ x = (\_ -> PwElement "button" dats [txt x])

button'__ :: Sig__ s opq
button'__ x = PwElement "button" dats [txt x]


canvas :: AFSig s opq
canvas x y = (\_ -> PwElement "canvas" x y)

canvas' :: Sig s opq
canvas' = PwElement "canvas"

canvas_ :: AFSig_ s opq
canvas_ x = (\_ -> PwElement "canvas" dats x)

canvas'_ :: Sig_ s opq
canvas'_ x = PwElement "canvas" dats x

canvas__ :: AFSig__ s opq
canvas__ x = (\_ -> PwElement "canvas" dats [txt x])

canvas'__ :: Sig__ s opq
canvas'__ x = PwElement "canvas" dats [txt x]


caption :: AFSig s opq
caption x y = (\_ -> PwElement "caption" x y)

caption' :: Sig s opq
caption' = PwElement "caption"

caption_ :: AFSig_ s opq
caption_ x = (\_ -> PwElement "caption" dats x)

caption'_ :: Sig_ s opq
caption'_ x = PwElement "caption" dats x

caption__ :: AFSig__ s opq
caption__ x = (\_ -> PwElement "caption" dats [txt x])

caption'__ :: Sig__ s opq
caption'__ x = PwElement "caption" dats [txt x]


center :: AFSig s opq
center x y = (\_ -> PwElement "center" x y)

center' :: Sig s opq
center' = PwElement "center"

center_ :: AFSig_ s opq
center_ x = (\_ -> PwElement "center" dats x)

center'_ :: Sig_ s opq
center'_ x = PwElement "center" dats x

center__ :: AFSig__ s opq
center__ x = (\_ -> PwElement "center" dats [txt x])

center'__ :: Sig__ s opq
center'__ x = PwElement "center" dats [txt x]


cite :: AFSig s opq
cite x y = (\_ -> PwElement "cite" x y)

cite' :: Sig s opq
cite' = PwElement "cite"

cite_ :: AFSig_ s opq
cite_ x = (\_ -> PwElement "cite" dats x)

cite'_ :: Sig_ s opq
cite'_ x = PwElement "cite" dats x

cite__ :: AFSig__ s opq
cite__ x = (\_ -> PwElement "cite" dats [txt x])

cite'__ :: Sig__ s opq
cite'__ x = PwElement "cite" dats [txt x]


code :: AFSig s opq
code x y = (\_ -> PwElement "code" x y)

code' :: Sig s opq
code' = PwElement "code"

code_ :: AFSig_ s opq
code_ x = (\_ -> PwElement "code" dats x)

code'_ :: Sig_ s opq
code'_ x = PwElement "code" dats x

code__ :: AFSig__ s opq
code__ x = (\_ -> PwElement "code" dats [txt x])

code'__ :: Sig__ s opq
code'__ x = PwElement "code" dats [txt x]


col :: AFSig s opq
col x y = (\_ -> PwElement "col" x y)

col' :: Sig s opq
col' = PwElement "col"

col_ :: AFSig_ s opq
col_ x = (\_ -> PwElement "col" dats x)

col'_ :: Sig_ s opq
col'_ x = PwElement "col" dats x

col__ :: AFSig__ s opq
col__ x = (\_ -> PwElement "col" dats [txt x])

col'__ :: Sig__ s opq
col'__ x = PwElement "col" dats [txt x]


colgroup :: AFSig s opq
colgroup x y = (\_ -> PwElement "colgroup" x y)

colgroup' :: Sig s opq
colgroup' = PwElement "colgroup"

colgroup_ :: AFSig_ s opq
colgroup_ x = (\_ -> PwElement "colgroup" dats x)

colgroup'_ :: Sig_ s opq
colgroup'_ x = PwElement "colgroup" dats x

colgroup__ :: AFSig__ s opq
colgroup__ x = (\_ -> PwElement "colgroup" dats [txt x])

colgroup'__ :: Sig__ s opq
colgroup'__ x = PwElement "colgroup" dats [txt x]


_data :: AFSig s opq
_data x y = (\_ -> PwElement "_data" x y)

_data' :: Sig s opq
_data' = PwElement "_data"

_data_ :: AFSig_ s opq
_data_ x = (\_ -> PwElement "_data" dats x)

_data'_ :: Sig_ s opq
_data'_ x = PwElement "_data" dats x

_data__ :: AFSig__ s opq
_data__ x = (\_ -> PwElement "_data" dats [txt x])

_data'__ :: Sig__ s opq
_data'__ x = PwElement "_data" dats [txt x]


_datalist :: AFSig s opq
_datalist x y = (\_ -> PwElement "_datalist" x y)

_datalist' :: Sig s opq
_datalist' = PwElement "_datalist"

_datalist_ :: AFSig_ s opq
_datalist_ x = (\_ -> PwElement "_datalist" dats x)

_datalist'_ :: Sig_ s opq
_datalist'_ x = PwElement "_datalist" dats x

_datalist__ :: AFSig__ s opq
_datalist__ x = (\_ -> PwElement "_datalist" dats [txt x])

_datalist'__ :: Sig__ s opq
_datalist'__ x = PwElement "_datalist" dats [txt x]


dd :: AFSig s opq
dd x y = (\_ -> PwElement "dd" x y)

dd' :: Sig s opq
dd' = PwElement "dd"

dd_ :: AFSig_ s opq
dd_ x = (\_ -> PwElement "dd" dats x)

dd'_ :: Sig_ s opq
dd'_ x = PwElement "dd" dats x

dd__ :: AFSig__ s opq
dd__ x = (\_ -> PwElement "dd" dats [txt x])

dd'__ :: Sig__ s opq
dd'__ x = PwElement "dd" dats [txt x]


del :: AFSig s opq
del x y = (\_ -> PwElement "del" x y)

del' :: Sig s opq
del' = PwElement "del"

del_ :: AFSig_ s opq
del_ x = (\_ -> PwElement "del" dats x)

del'_ :: Sig_ s opq
del'_ x = PwElement "del" dats x

del__ :: AFSig__ s opq
del__ x = (\_ -> PwElement "del" dats [txt x])

del'__ :: Sig__ s opq
del'__ x = PwElement "del" dats [txt x]


details :: AFSig s opq
details x y = (\_ -> PwElement "details" x y)

details' :: Sig s opq
details' = PwElement "details"

details_ :: AFSig_ s opq
details_ x = (\_ -> PwElement "details" dats x)

details'_ :: Sig_ s opq
details'_ x = PwElement "details" dats x

details__ :: AFSig__ s opq
details__ x = (\_ -> PwElement "details" dats [txt x])

details'__ :: Sig__ s opq
details'__ x = PwElement "details" dats [txt x]


dfn :: AFSig s opq
dfn x y = (\_ -> PwElement "dfn" x y)

dfn' :: Sig s opq
dfn' = PwElement "dfn"

dfn_ :: AFSig_ s opq
dfn_ x = (\_ -> PwElement "dfn" dats x)

dfn'_ :: Sig_ s opq
dfn'_ x = PwElement "dfn" dats x

dfn__ :: AFSig__ s opq
dfn__ x = (\_ -> PwElement "dfn" dats [txt x])

dfn'__ :: Sig__ s opq
dfn'__ x = PwElement "dfn" dats [txt x]


dialog :: AFSig s opq
dialog x y = (\_ -> PwElement "dialog" x y)

dialog' :: Sig s opq
dialog' = PwElement "dialog"

dialog_ :: AFSig_ s opq
dialog_ x = (\_ -> PwElement "dialog" dats x)

dialog'_ :: Sig_ s opq
dialog'_ x = PwElement "dialog" dats x

dialog__ :: AFSig__ s opq
dialog__ x = (\_ -> PwElement "dialog" dats [txt x])

dialog'__ :: Sig__ s opq
dialog'__ x = PwElement "dialog" dats [txt x]


dir :: AFSig s opq
dir x y = (\_ -> PwElement "dir" x y)

dir' :: Sig s opq
dir' = PwElement "dir"

dir_ :: AFSig_ s opq
dir_ x = (\_ -> PwElement "dir" dats x)

dir'_ :: Sig_ s opq
dir'_ x = PwElement "dir" dats x

dir__ :: AFSig__ s opq
dir__ x = (\_ -> PwElement "dir" dats [txt x])

dir'__ :: Sig__ s opq
dir'__ x = PwElement "dir" dats [txt x]


div :: AFSig s opq
div x y = (\_ -> PwElement "div" x y)

div' :: Sig s opq
div' = PwElement "div"

div_ :: AFSig_ s opq
div_ x = (\_ -> PwElement "div" dats x)

div'_ :: Sig_ s opq
div'_ x = PwElement "div" dats x

div__ :: AFSig__ s opq
div__ x = (\_ -> PwElement "div" dats [txt x])

div'__ :: Sig__ s opq
div'__ x = PwElement "div" dats [txt x]


dl :: AFSig s opq
dl x y = (\_ -> PwElement "dl" x y)

dl' :: Sig s opq
dl' = PwElement "dl"

dl_ :: AFSig_ s opq
dl_ x = (\_ -> PwElement "dl" dats x)

dl'_ :: Sig_ s opq
dl'_ x = PwElement "dl" dats x

dl__ :: AFSig__ s opq
dl__ x = (\_ -> PwElement "dl" dats [txt x])

dl'__ :: Sig__ s opq
dl'__ x = PwElement "dl" dats [txt x]


dt :: AFSig s opq
dt x y = (\_ -> PwElement "dt" x y)

dt' :: Sig s opq
dt' = PwElement "dt"

dt_ :: AFSig_ s opq
dt_ x = (\_ -> PwElement "dt" dats x)

dt'_ :: Sig_ s opq
dt'_ x = PwElement "dt" dats x

dt__ :: AFSig__ s opq
dt__ x = (\_ -> PwElement "dt" dats [txt x])

dt'__ :: Sig__ s opq
dt'__ x = PwElement "dt" dats [txt x]


em :: AFSig s opq
em x y = (\_ -> PwElement "em" x y)

em' :: Sig s opq
em' = PwElement "em"

em_ :: AFSig_ s opq
em_ x = (\_ -> PwElement "em" dats x)

em'_ :: Sig_ s opq
em'_ x = PwElement "em" dats x

em__ :: AFSig__ s opq
em__ x = (\_ -> PwElement "em" dats [txt x])

em'__ :: Sig__ s opq
em'__ x = PwElement "em" dats [txt x]


embed :: AFSig s opq
embed x y = (\_ -> PwElement "embed" x y)

embed' :: Sig s opq
embed' = PwElement "embed"

embed_ :: AFSig_ s opq
embed_ x = (\_ -> PwElement "embed" dats x)

embed'_ :: Sig_ s opq
embed'_ x = PwElement "embed" dats x

embed__ :: AFSig__ s opq
embed__ x = (\_ -> PwElement "embed" dats [txt x])

embed'__ :: Sig__ s opq
embed'__ x = PwElement "embed" dats [txt x]


fieldset :: AFSig s opq
fieldset x y = (\_ -> PwElement "fieldset" x y)

fieldset' :: Sig s opq
fieldset' = PwElement "fieldset"

fieldset_ :: AFSig_ s opq
fieldset_ x = (\_ -> PwElement "fieldset" dats x)

fieldset'_ :: Sig_ s opq
fieldset'_ x = PwElement "fieldset" dats x

fieldset__ :: AFSig__ s opq
fieldset__ x = (\_ -> PwElement "fieldset" dats [txt x])

fieldset'__ :: Sig__ s opq
fieldset'__ x = PwElement "fieldset" dats [txt x]


figcaption :: AFSig s opq
figcaption x y = (\_ -> PwElement "figcaption" x y)

figcaption' :: Sig s opq
figcaption' = PwElement "figcaption"

figcaption_ :: AFSig_ s opq
figcaption_ x = (\_ -> PwElement "figcaption" dats x)

figcaption'_ :: Sig_ s opq
figcaption'_ x = PwElement "figcaption" dats x

figcaption__ :: AFSig__ s opq
figcaption__ x = (\_ -> PwElement "figcaption" dats [txt x])

figcaption'__ :: Sig__ s opq
figcaption'__ x = PwElement "figcaption" dats [txt x]


figure :: AFSig s opq
figure x y = (\_ -> PwElement "figure" x y)

figure' :: Sig s opq
figure' = PwElement "figure"

figure_ :: AFSig_ s opq
figure_ x = (\_ -> PwElement "figure" dats x)

figure'_ :: Sig_ s opq
figure'_ x = PwElement "figure" dats x

figure__ :: AFSig__ s opq
figure__ x = (\_ -> PwElement "figure" dats [txt x])

figure'__ :: Sig__ s opq
figure'__ x = PwElement "figure" dats [txt x]


font :: AFSig s opq
font x y = (\_ -> PwElement "font" x y)

font' :: Sig s opq
font' = PwElement "font"

font_ :: AFSig_ s opq
font_ x = (\_ -> PwElement "font" dats x)

font'_ :: Sig_ s opq
font'_ x = PwElement "font" dats x

font__ :: AFSig__ s opq
font__ x = (\_ -> PwElement "font" dats [txt x])

font'__ :: Sig__ s opq
font'__ x = PwElement "font" dats [txt x]


footer :: AFSig s opq
footer x y = (\_ -> PwElement "footer" x y)

footer' :: Sig s opq
footer' = PwElement "footer"

footer_ :: AFSig_ s opq
footer_ x = (\_ -> PwElement "footer" dats x)

footer'_ :: Sig_ s opq
footer'_ x = PwElement "footer" dats x

footer__ :: AFSig__ s opq
footer__ x = (\_ -> PwElement "footer" dats [txt x])

footer'__ :: Sig__ s opq
footer'__ x = PwElement "footer" dats [txt x]


form :: AFSig s opq
form x y = (\_ -> PwElement "form" x y)

form' :: Sig s opq
form' = PwElement "form"

form_ :: AFSig_ s opq
form_ x = (\_ -> PwElement "form" dats x)

form'_ :: Sig_ s opq
form'_ x = PwElement "form" dats x

form__ :: AFSig__ s opq
form__ x = (\_ -> PwElement "form" dats [txt x])

form'__ :: Sig__ s opq
form'__ x = PwElement "form" dats [txt x]


frame :: AFSig s opq
frame x y = (\_ -> PwElement "frame" x y)

frame' :: Sig s opq
frame' = PwElement "frame"

frame_ :: AFSig_ s opq
frame_ x = (\_ -> PwElement "frame" dats x)

frame'_ :: Sig_ s opq
frame'_ x = PwElement "frame" dats x

frame__ :: AFSig__ s opq
frame__ x = (\_ -> PwElement "frame" dats [txt x])

frame'__ :: Sig__ s opq
frame'__ x = PwElement "frame" dats [txt x]


frameset :: AFSig s opq
frameset x y = (\_ -> PwElement "frameset" x y)

frameset' :: Sig s opq
frameset' = PwElement "frameset"

frameset_ :: AFSig_ s opq
frameset_ x = (\_ -> PwElement "frameset" dats x)

frameset'_ :: Sig_ s opq
frameset'_ x = PwElement "frameset" dats x

frameset__ :: AFSig__ s opq
frameset__ x = (\_ -> PwElement "frameset" dats [txt x])

frameset'__ :: Sig__ s opq
frameset'__ x = PwElement "frameset" dats [txt x]

head :: AFSig s opq
head x y = (\_ -> PwElement "head" x y)

head' :: Sig s opq
head' = PwElement "head"

head_ :: AFSig_ s opq
head_ x = (\_ -> PwElement "head" dats x)

head'_ :: Sig_ s opq
head'_ x = PwElement "head" dats x

head__ :: AFSig__ s opq
head__ x = (\_ -> PwElement "head" dats [txt x])

head'__ :: Sig__ s opq
head'__ x = PwElement "head" dats [txt x]


header :: AFSig s opq
header x y = (\_ -> PwElement "header" x y)

header' :: Sig s opq
header' = PwElement "header"

header_ :: AFSig_ s opq
header_ x = (\_ -> PwElement "header" dats x)

header'_ :: Sig_ s opq
header'_ x = PwElement "header" dats x

header__ :: AFSig__ s opq
header__ x = (\_ -> PwElement "header" dats [txt x])

header'__ :: Sig__ s opq
header'__ x = PwElement "header" dats [txt x]


hr :: (s -> PwNode s opq)
hr = (\_ -> PwElement "br" dats [])

html :: AFSig s opq
html x y = (\_ -> PwElement "html" x y)

html' :: Sig s opq
html' = PwElement "html"

html_ :: AFSig_ s opq
html_ x = (\_ -> PwElement "html" dats x)

html'_ :: Sig_ s opq
html'_ x = PwElement "html" dats x

html__ :: AFSig__ s opq
html__ x = (\_ -> PwElement "html" dats [txt x])

html'__ :: Sig__ s opq
html'__ x = PwElement "html" dats [txt x]


i :: AFSig s opq
i x y = (\_ -> PwElement "i" x y)

i' :: Sig s opq
i' = PwElement "i"

i_ :: AFSig_ s opq
i_ x = (\_ -> PwElement "i" dats x)

i'_ :: Sig_ s opq
i'_ x = PwElement "i" dats x

i__ :: AFSig__ s opq
i__ x = (\_ -> PwElement "i" dats [txt x])

i'__ :: Sig__ s opq
i'__ x = PwElement "i" dats [txt x]


iframe :: AFSig s opq
iframe x y = (\_ -> PwElement "iframe" x y)

iframe' :: Sig s opq
iframe' = PwElement "iframe"

iframe_ :: AFSig_ s opq
iframe_ x = (\_ -> PwElement "iframe" dats x)

iframe'_ :: Sig_ s opq
iframe'_ x = PwElement "iframe" dats x

iframe__ :: AFSig__ s opq
iframe__ x = (\_ -> PwElement "iframe" dats [txt x])

iframe'__ :: Sig__ s opq
iframe'__ x = PwElement "iframe" dats [txt x]


img :: [(String, (s -> PwAttribute s opq))] -> (s -> PwNode s opq)
img x = (\_ -> PwElement "img" x [])

img' :: [(String, (s -> PwAttribute s opq))] -> PwNode s opq
img' x = PwElement "img" x []

img_ :: (s -> PwNode s opq)
img_ = (\_ -> PwElement "img" dats [])

img'_ :: PwNode s opq
img'_ = PwElement "img" dats []

input :: AFSig s opq
input x y = (\_ -> PwElement "input" x y)

input' :: Sig s opq
input' = PwElement "input"

input_ :: AFSig_ s opq
input_ x = (\_ -> PwElement "input" dats x)

input'_ :: Sig_ s opq
input'_ x = PwElement "input" dats x

input__ :: AFSig__ s opq
input__ x = (\_ -> PwElement "input" dats [txt x])

input'__ :: Sig__ s opq
input'__ x = PwElement "input" dats [txt x]


ins :: AFSig s opq
ins x y = (\_ -> PwElement "ins" x y)

ins' :: Sig s opq
ins' = PwElement "ins"

ins_ :: AFSig_ s opq
ins_ x = (\_ -> PwElement "ins" dats x)

ins'_ :: Sig_ s opq
ins'_ x = PwElement "ins" dats x

ins__ :: AFSig__ s opq
ins__ x = (\_ -> PwElement "ins" dats [txt x])

ins'__ :: Sig__ s opq
ins'__ x = PwElement "ins" dats [txt x]


kbd :: AFSig s opq
kbd x y = (\_ -> PwElement "kbd" x y)

kbd' :: Sig s opq
kbd' = PwElement "kbd"

kbd_ :: AFSig_ s opq
kbd_ x = (\_ -> PwElement "kbd" dats x)

kbd'_ :: Sig_ s opq
kbd'_ x = PwElement "kbd" dats x

kbd__ :: AFSig__ s opq
kbd__ x = (\_ -> PwElement "kbd" dats [txt x])

kbd'__ :: Sig__ s opq
kbd'__ x = PwElement "kbd" dats [txt x]


label :: AFSig s opq
label x y = (\_ -> PwElement "label" x y)

label' :: Sig s opq
label' = PwElement "label"

label_ :: AFSig_ s opq
label_ x = (\_ -> PwElement "label" dats x)

label'_ :: Sig_ s opq
label'_ x = PwElement "label" dats x

label__ :: AFSig__ s opq
label__ x = (\_ -> PwElement "label" dats [txt x])

label'__ :: Sig__ s opq
label'__ x = PwElement "label" dats [txt x]


legend :: AFSig s opq
legend x y = (\_ -> PwElement "legend" x y)

legend' :: Sig s opq
legend' = PwElement "legend"

legend_ :: AFSig_ s opq
legend_ x = (\_ -> PwElement "legend" dats x)

legend'_ :: Sig_ s opq
legend'_ x = PwElement "legend" dats x

legend__ :: AFSig__ s opq
legend__ x = (\_ -> PwElement "legend" dats [txt x])

legend'__ :: Sig__ s opq
legend'__ x = PwElement "legend" dats [txt x]


li :: AFSig s opq
li x y = (\_ -> PwElement "li" x y)

li' :: Sig s opq
li' = PwElement "li"

li_ :: AFSig_ s opq
li_ x = (\_ -> PwElement "li" dats x)

li'_ :: Sig_ s opq
li'_ x = PwElement "li" dats x

li__ :: AFSig__ s opq
li__ x = (\_ -> PwElement "li" dats [txt x])

li'__ :: Sig__ s opq
li'__ x = PwElement "li" dats [txt x]


link :: AFSig s opq
link x y = (\_ -> PwElement "link" x y)

link' :: Sig s opq
link' = PwElement "link"

link_ :: AFSig_ s opq
link_ x = (\_ -> PwElement "link" dats x)

link'_ :: Sig_ s opq
link'_ x = PwElement "link" dats x

link__ :: AFSig__ s opq
link__ x = (\_ -> PwElement "link" dats [txt x])

link'__ :: Sig__ s opq
link'__ x = PwElement "link" dats [txt x]


main :: AFSig s opq
main x y = (\_ -> PwElement "main" x y)

main' :: Sig s opq
main' = PwElement "main"

main_ :: AFSig_ s opq
main_ x = (\_ -> PwElement "main" dats x)

main'_ :: Sig_ s opq
main'_ x = PwElement "main" dats x

main__ :: AFSig__ s opq
main__ x = (\_ -> PwElement "main" dats [txt x])

main'__ :: Sig__ s opq
main'__ x = PwElement "main" dats [txt x]


map :: AFSig s opq
map x y = (\_ -> PwElement "map" x y)

map' :: Sig s opq
map' = PwElement "map"

map_ :: AFSig_ s opq
map_ x = (\_ -> PwElement "map" dats x)

map'_ :: Sig_ s opq
map'_ x = PwElement "map" dats x

map__ :: AFSig__ s opq
map__ x = (\_ -> PwElement "map" dats [txt x])

map'__ :: Sig__ s opq
map'__ x = PwElement "map" dats [txt x]


mark :: AFSig s opq
mark x y = (\_ -> PwElement "mark" x y)

mark' :: Sig s opq
mark' = PwElement "mark"

mark_ :: AFSig_ s opq
mark_ x = (\_ -> PwElement "mark" dats x)

mark'_ :: Sig_ s opq
mark'_ x = PwElement "mark" dats x

mark__ :: AFSig__ s opq
mark__ x = (\_ -> PwElement "mark" dats [txt x])

mark'__ :: Sig__ s opq
mark'__ x = PwElement "mark" dats [txt x]


meta :: AFSig s opq
meta x y = (\_ -> PwElement "meta" x y)

meta' :: Sig s opq
meta' = PwElement "meta"

meta_ :: AFSig_ s opq
meta_ x = (\_ -> PwElement "meta" dats x)

meta'_ :: Sig_ s opq
meta'_ x = PwElement "meta" dats x

meta__ :: AFSig__ s opq
meta__ x = (\_ -> PwElement "meta" dats [txt x])

meta'__ :: Sig__ s opq
meta'__ x = PwElement "meta" dats [txt x]


meter :: AFSig s opq
meter x y = (\_ -> PwElement "meter" x y)

meter' :: Sig s opq
meter' = PwElement "meter"

meter_ :: AFSig_ s opq
meter_ x = (\_ -> PwElement "meter" dats x)

meter'_ :: Sig_ s opq
meter'_ x = PwElement "meter" dats x

meter__ :: AFSig__ s opq
meter__ x = (\_ -> PwElement "meter" dats [txt x])

meter'__ :: Sig__ s opq
meter'__ x = PwElement "meter" dats [txt x]


nav :: AFSig s opq
nav x y = (\_ -> PwElement "nav" x y)

nav' :: Sig s opq
nav' = PwElement "nav"

nav_ :: AFSig_ s opq
nav_ x = (\_ -> PwElement "nav" dats x)

nav'_ :: Sig_ s opq
nav'_ x = PwElement "nav" dats x

nav__ :: AFSig__ s opq
nav__ x = (\_ -> PwElement "nav" dats [txt x])

nav'__ :: Sig__ s opq
nav'__ x = PwElement "nav" dats [txt x]


noframes :: AFSig s opq
noframes x y = (\_ -> PwElement "noframes" x y)

noframes' :: Sig s opq
noframes' = PwElement "noframes"

noframes_ :: AFSig_ s opq
noframes_ x = (\_ -> PwElement "noframes" dats x)

noframes'_ :: Sig_ s opq
noframes'_ x = PwElement "noframes" dats x

noframes__ :: AFSig__ s opq
noframes__ x = (\_ -> PwElement "noframes" dats [txt x])

noframes'__ :: Sig__ s opq
noframes'__ x = PwElement "noframes" dats [txt x]


noscript :: AFSig s opq
noscript x y = (\_ -> PwElement "noscript" x y)

noscript' :: Sig s opq
noscript' = PwElement "noscript"

noscript_ :: AFSig_ s opq
noscript_ x = (\_ -> PwElement "noscript" dats x)

noscript'_ :: Sig_ s opq
noscript'_ x = PwElement "noscript" dats x

noscript__ :: AFSig__ s opq
noscript__ x = (\_ -> PwElement "noscript" dats [txt x])

noscript'__ :: Sig__ s opq
noscript'__ x = PwElement "noscript" dats [txt x]


object :: AFSig s opq
object x y = (\_ -> PwElement "object" x y)

object' :: Sig s opq
object' = PwElement "object"

object_ :: AFSig_ s opq
object_ x = (\_ -> PwElement "object" dats x)

object'_ :: Sig_ s opq
object'_ x = PwElement "object" dats x

object__ :: AFSig__ s opq
object__ x = (\_ -> PwElement "object" dats [txt x])

object'__ :: Sig__ s opq
object'__ x = PwElement "object" dats [txt x]


ol :: AFSig s opq
ol x y = (\_ -> PwElement "ol" x y)

ol' :: Sig s opq
ol' = PwElement "ol"

ol_ :: AFSig_ s opq
ol_ x = (\_ -> PwElement "ol" dats x)

ol'_ :: Sig_ s opq
ol'_ x = PwElement "ol" dats x

ol__ :: AFSig__ s opq
ol__ x = (\_ -> PwElement "ol" dats [txt x])

ol'__ :: Sig__ s opq
ol'__ x = PwElement "ol" dats [txt x]


optgroup :: AFSig s opq
optgroup x y = (\_ -> PwElement "optgroup" x y)

optgroup' :: Sig s opq
optgroup' = PwElement "optgroup"

optgroup_ :: AFSig_ s opq
optgroup_ x = (\_ -> PwElement "optgroup" dats x)

optgroup'_ :: Sig_ s opq
optgroup'_ x = PwElement "optgroup" dats x

optgroup__ :: AFSig__ s opq
optgroup__ x = (\_ -> PwElement "optgroup" dats [txt x])

optgroup'__ :: Sig__ s opq
optgroup'__ x = PwElement "optgroup" dats [txt x]


option :: AFSig s opq
option x y = (\_ -> PwElement "option" x y)

option' :: Sig s opq
option' = PwElement "option"

option_ :: AFSig_ s opq
option_ x = (\_ -> PwElement "option" dats x)

option'_ :: Sig_ s opq
option'_ x = PwElement "option" dats x

option__ :: AFSig__ s opq
option__ x = (\_ -> PwElement "option" dats [txt x])

option'__ :: Sig__ s opq
option'__ x = PwElement "option" dats [txt x]


output :: AFSig s opq
output x y = (\_ -> PwElement "output" x y)

output' :: Sig s opq
output' = PwElement "output"

output_ :: AFSig_ s opq
output_ x = (\_ -> PwElement "output" dats x)

output'_ :: Sig_ s opq
output'_ x = PwElement "output" dats x

output__ :: AFSig__ s opq
output__ x = (\_ -> PwElement "output" dats [txt x])

output'__ :: Sig__ s opq
output'__ x = PwElement "output" dats [txt x]


p :: AFSig s opq
p x y = (\_ -> PwElement "p" x y)

p' :: Sig s opq
p' = PwElement "p"

p_ :: AFSig_ s opq
p_ x = (\_ -> PwElement "p" dats x)

p'_ :: Sig_ s opq
p'_ x = PwElement "p" dats x

p__ :: AFSig__ s opq
p__ x = (\_ -> PwElement "p" dats [txt x])

p'__ :: Sig__ s opq
p'__ x = PwElement "p" dats [txt x]


param :: AFSig s opq
param x y = (\_ -> PwElement "param" x y)

param' :: Sig s opq
param' = PwElement "param"

param_ :: AFSig_ s opq
param_ x = (\_ -> PwElement "param" dats x)

param'_ :: Sig_ s opq
param'_ x = PwElement "param" dats x

param__ :: AFSig__ s opq
param__ x = (\_ -> PwElement "param" dats [txt x])

param'__ :: Sig__ s opq
param'__ x = PwElement "param" dats [txt x]


picture :: AFSig s opq
picture x y = (\_ -> PwElement "picture" x y)

picture' :: Sig s opq
picture' = PwElement "picture"

picture_ :: AFSig_ s opq
picture_ x = (\_ -> PwElement "picture" dats x)

picture'_ :: Sig_ s opq
picture'_ x = PwElement "picture" dats x

picture__ :: AFSig__ s opq
picture__ x = (\_ -> PwElement "picture" dats [txt x])

picture'__ :: Sig__ s opq
picture'__ x = PwElement "picture" dats [txt x]


pre :: AFSig s opq
pre x y = (\_ -> PwElement "pre" x y)

pre' :: Sig s opq
pre' = PwElement "pre"

pre_ :: AFSig_ s opq
pre_ x = (\_ -> PwElement "pre" dats x)

pre'_ :: Sig_ s opq
pre'_ x = PwElement "pre" dats x

pre__ :: AFSig__ s opq
pre__ x = (\_ -> PwElement "pre" dats [txt x])

pre'__ :: Sig__ s opq
pre'__ x = PwElement "pre" dats [txt x]


progress :: AFSig s opq
progress x y = (\_ -> PwElement "progress" x y)

progress' :: Sig s opq
progress' = PwElement "progress"

progress_ :: AFSig_ s opq
progress_ x = (\_ -> PwElement "progress" dats x)

progress'_ :: Sig_ s opq
progress'_ x = PwElement "progress" dats x

progress__ :: AFSig__ s opq
progress__ x = (\_ -> PwElement "progress" dats [txt x])

progress'__ :: Sig__ s opq
progress'__ x = PwElement "progress" dats [txt x]


q :: AFSig s opq
q x y = (\_ -> PwElement "q" x y)

q' :: Sig s opq
q' = PwElement "q"

q_ :: AFSig_ s opq
q_ x = (\_ -> PwElement "q" dats x)

q'_ :: Sig_ s opq
q'_ x = PwElement "q" dats x

q__ :: AFSig__ s opq
q__ x = (\_ -> PwElement "q" dats [txt x])

q'__ :: Sig__ s opq
q'__ x = PwElement "q" dats [txt x]


rp :: AFSig s opq
rp x y = (\_ -> PwElement "rp" x y)

rp' :: Sig s opq
rp' = PwElement "rp"

rp_ :: AFSig_ s opq
rp_ x = (\_ -> PwElement "rp" dats x)

rp'_ :: Sig_ s opq
rp'_ x = PwElement "rp" dats x

rp__ :: AFSig__ s opq
rp__ x = (\_ -> PwElement "rp" dats [txt x])

rp'__ :: Sig__ s opq
rp'__ x = PwElement "rp" dats [txt x]


rt :: AFSig s opq
rt x y = (\_ -> PwElement "rt" x y)

rt' :: Sig s opq
rt' = PwElement "rt"

rt_ :: AFSig_ s opq
rt_ x = (\_ -> PwElement "rt" dats x)

rt'_ :: Sig_ s opq
rt'_ x = PwElement "rt" dats x

rt__ :: AFSig__ s opq
rt__ x = (\_ -> PwElement "rt" dats [txt x])

rt'__ :: Sig__ s opq
rt'__ x = PwElement "rt" dats [txt x]


ruby :: AFSig s opq
ruby x y = (\_ -> PwElement "ruby" x y)

ruby' :: Sig s opq
ruby' = PwElement "ruby"

ruby_ :: AFSig_ s opq
ruby_ x = (\_ -> PwElement "ruby" dats x)

ruby'_ :: Sig_ s opq
ruby'_ x = PwElement "ruby" dats x

ruby__ :: AFSig__ s opq
ruby__ x = (\_ -> PwElement "ruby" dats [txt x])

ruby'__ :: Sig__ s opq
ruby'__ x = PwElement "ruby" dats [txt x]


s :: AFSig s opq
s x y = (\_ -> PwElement "s" x y)

s' :: Sig s opq
s' = PwElement "s"

s_ :: AFSig_ s opq
s_ x = (\_ -> PwElement "s" dats x)

s'_ :: Sig_ s opq
s'_ x = PwElement "s" dats x

s__ :: AFSig__ s opq
s__ x = (\_ -> PwElement "s" dats [txt x])

s'__ :: Sig__ s opq
s'__ x = PwElement "s" dats [txt x]


samp :: AFSig s opq
samp x y = (\_ -> PwElement "samp" x y)

samp' :: Sig s opq
samp' = PwElement "samp"

samp_ :: AFSig_ s opq
samp_ x = (\_ -> PwElement "samp" dats x)

samp'_ :: Sig_ s opq
samp'_ x = PwElement "samp" dats x

samp__ :: AFSig__ s opq
samp__ x = (\_ -> PwElement "samp" dats [txt x])

samp'__ :: Sig__ s opq
samp'__ x = PwElement "samp" dats [txt x]


script :: AFSig s opq
script x y = (\_ -> PwElement "script" x y)

script' :: Sig s opq
script' = PwElement "script"

script_ :: AFSig_ s opq
script_ x = (\_ -> PwElement "script" dats x)

script'_ :: Sig_ s opq
script'_ x = PwElement "script" dats x

script__ :: AFSig__ s opq
script__ x = (\_ -> PwElement "script" dats [txt x])

script'__ :: Sig__ s opq
script'__ x = PwElement "script" dats [txt x]


section :: AFSig s opq
section x y = (\_ -> PwElement "section" x y)

section' :: Sig s opq
section' = PwElement "section"

section_ :: AFSig_ s opq
section_ x = (\_ -> PwElement "section" dats x)

section'_ :: Sig_ s opq
section'_ x = PwElement "section" dats x

section__ :: AFSig__ s opq
section__ x = (\_ -> PwElement "section" dats [txt x])

section'__ :: Sig__ s opq
section'__ x = PwElement "section" dats [txt x]


select :: AFSig s opq
select x y = (\_ -> PwElement "select" x y)

select' :: Sig s opq
select' = PwElement "select"

select_ :: AFSig_ s opq
select_ x = (\_ -> PwElement "select" dats x)

select'_ :: Sig_ s opq
select'_ x = PwElement "select" dats x

select__ :: AFSig__ s opq
select__ x = (\_ -> PwElement "select" dats [txt x])

select'__ :: Sig__ s opq
select'__ x = PwElement "select" dats [txt x]


small :: AFSig s opq
small x y = (\_ -> PwElement "small" x y)

small' :: Sig s opq
small' = PwElement "small"

small_ :: AFSig_ s opq
small_ x = (\_ -> PwElement "small" dats x)

small'_ :: Sig_ s opq
small'_ x = PwElement "small" dats x

small__ :: AFSig__ s opq
small__ x = (\_ -> PwElement "small" dats [txt x])

small'__ :: Sig__ s opq
small'__ x = PwElement "small" dats [txt x]


source :: AFSig s opq
source x y = (\_ -> PwElement "source" x y)

source' :: Sig s opq
source' = PwElement "source"

source_ :: AFSig_ s opq
source_ x = (\_ -> PwElement "source" dats x)

source'_ :: Sig_ s opq
source'_ x = PwElement "source" dats x

source__ :: AFSig__ s opq
source__ x = (\_ -> PwElement "source" dats [txt x])

source'__ :: Sig__ s opq
source'__ x = PwElement "source" dats [txt x]


span :: AFSig s opq
span x y = (\_ -> PwElement "span" x y)

span' :: Sig s opq
span' = PwElement "span"

span_ :: AFSig_ s opq
span_ x = (\_ -> PwElement "span" dats x)

span'_ :: Sig_ s opq
span'_ x = PwElement "span" dats x

span__ :: AFSig__ s opq
span__ x = (\_ -> PwElement "span" dats [txt x])

span'__ :: Sig__ s opq
span'__ x = PwElement "span" dats [txt x]


strike :: AFSig s opq
strike x y = (\_ -> PwElement "strike" x y)

strike' :: Sig s opq
strike' = PwElement "strike"

strike_ :: AFSig_ s opq
strike_ x = (\_ -> PwElement "strike" dats x)

strike'_ :: Sig_ s opq
strike'_ x = PwElement "strike" dats x

strike__ :: AFSig__ s opq
strike__ x = (\_ -> PwElement "strike" dats [txt x])

strike'__ :: Sig__ s opq
strike'__ x = PwElement "strike" dats [txt x]


strong :: AFSig s opq
strong x y = (\_ -> PwElement "strong" x y)

strong' :: Sig s opq
strong' = PwElement "strong"

strong_ :: AFSig_ s opq
strong_ x = (\_ -> PwElement "strong" dats x)

strong'_ :: Sig_ s opq
strong'_ x = PwElement "strong" dats x

strong__ :: AFSig__ s opq
strong__ x = (\_ -> PwElement "strong" dats [txt x])

strong'__ :: Sig__ s opq
strong'__ x = PwElement "strong" dats [txt x]


style :: AFSig s opq
style x y = (\_ -> PwElement "style" x y)

style' :: Sig s opq
style' = PwElement "style"

style_ :: AFSig_ s opq
style_ x = (\_ -> PwElement "style" dats x)

style'_ :: Sig_ s opq
style'_ x = PwElement "style" dats x

style__ :: AFSig__ s opq
style__ x = (\_ -> PwElement "style" dats [txt x])

style'__ :: Sig__ s opq
style'__ x = PwElement "style" dats [txt x]


sub :: AFSig s opq
sub x y = (\_ -> PwElement "sub" x y)

sub' :: Sig s opq
sub' = PwElement "sub"

sub_ :: AFSig_ s opq
sub_ x = (\_ -> PwElement "sub" dats x)

sub'_ :: Sig_ s opq
sub'_ x = PwElement "sub" dats x

sub__ :: AFSig__ s opq
sub__ x = (\_ -> PwElement "sub" dats [txt x])

sub'__ :: Sig__ s opq
sub'__ x = PwElement "sub" dats [txt x]


summary :: AFSig s opq
summary x y = (\_ -> PwElement "summary" x y)

summary' :: Sig s opq
summary' = PwElement "summary"

summary_ :: AFSig_ s opq
summary_ x = (\_ -> PwElement "summary" dats x)

summary'_ :: Sig_ s opq
summary'_ x = PwElement "summary" dats x

summary__ :: AFSig__ s opq
summary__ x = (\_ -> PwElement "summary" dats [txt x])

summary'__ :: Sig__ s opq
summary'__ x = PwElement "summary" dats [txt x]


sup :: AFSig s opq
sup x y = (\_ -> PwElement "sup" x y)

sup' :: Sig s opq
sup' = PwElement "sup"

sup_ :: AFSig_ s opq
sup_ x = (\_ -> PwElement "sup" dats x)

sup'_ :: Sig_ s opq
sup'_ x = PwElement "sup" dats x

sup__ :: AFSig__ s opq
sup__ x = (\_ -> PwElement "sup" dats [txt x])

sup'__ :: Sig__ s opq
sup'__ x = PwElement "sup" dats [txt x]


svg :: AFSig s opq
svg x y = (\_ -> PwElement "svg" x y)

svg' :: Sig s opq
svg' = PwElement "svg"

svg_ :: AFSig_ s opq
svg_ x = (\_ -> PwElement "svg" dats x)

svg'_ :: Sig_ s opq
svg'_ x = PwElement "svg" dats x

svg__ :: AFSig__ s opq
svg__ x = (\_ -> PwElement "svg" dats [txt x])

svg'__ :: Sig__ s opq
svg'__ x = PwElement "svg" dats [txt x]


table :: AFSig s opq
table x y = (\_ -> PwElement "table" x y)

table' :: Sig s opq
table' = PwElement "table"

table_ :: AFSig_ s opq
table_ x = (\_ -> PwElement "table" dats x)

table'_ :: Sig_ s opq
table'_ x = PwElement "table" dats x

table__ :: AFSig__ s opq
table__ x = (\_ -> PwElement "table" dats [txt x])

table'__ :: Sig__ s opq
table'__ x = PwElement "table" dats [txt x]


tbody :: AFSig s opq
tbody x y = (\_ -> PwElement "tbody" x y)

tbody' :: Sig s opq
tbody' = PwElement "tbody"

tbody_ :: AFSig_ s opq
tbody_ x = (\_ -> PwElement "tbody" dats x)

tbody'_ :: Sig_ s opq
tbody'_ x = PwElement "tbody" dats x

tbody__ :: AFSig__ s opq
tbody__ x = (\_ -> PwElement "tbody" dats [txt x])

tbody'__ :: Sig__ s opq
tbody'__ x = PwElement "tbody" dats [txt x]


td :: AFSig s opq
td x y = (\_ -> PwElement "td" x y)

td' :: Sig s opq
td' = PwElement "td"

td_ :: AFSig_ s opq
td_ x = (\_ -> PwElement "td" dats x)

td'_ :: Sig_ s opq
td'_ x = PwElement "td" dats x

td__ :: AFSig__ s opq
td__ x = (\_ -> PwElement "td" dats [txt x])

td'__ :: Sig__ s opq
td'__ x = PwElement "td" dats [txt x]


template :: AFSig s opq
template x y = (\_ -> PwElement "template" x y)

template' :: Sig s opq
template' = PwElement "template"

template_ :: AFSig_ s opq
template_ x = (\_ -> PwElement "template" dats x)

template'_ :: Sig_ s opq
template'_ x = PwElement "template" dats x

template__ :: AFSig__ s opq
template__ x = (\_ -> PwElement "template" dats [txt x])

template'__ :: Sig__ s opq
template'__ x = PwElement "template" dats [txt x]


textarea :: AFSig s opq
textarea x y = (\_ -> PwElement "textarea" x y)

textarea' :: Sig s opq
textarea' = PwElement "textarea"

textarea_ :: AFSig_ s opq
textarea_ x = (\_ -> PwElement "textarea" dats x)

textarea'_ :: Sig_ s opq
textarea'_ x = PwElement "textarea" dats x

textarea__ :: AFSig__ s opq
textarea__ x = (\_ -> PwElement "textarea" dats [txt x])

textarea'__ :: Sig__ s opq
textarea'__ x = PwElement "textarea" dats [txt x]


tfoot :: AFSig s opq
tfoot x y = (\_ -> PwElement "tfoot" x y)

tfoot' :: Sig s opq
tfoot' = PwElement "tfoot"

tfoot_ :: AFSig_ s opq
tfoot_ x = (\_ -> PwElement "tfoot" dats x)

tfoot'_ :: Sig_ s opq
tfoot'_ x = PwElement "tfoot" dats x

tfoot__ :: AFSig__ s opq
tfoot__ x = (\_ -> PwElement "tfoot" dats [txt x])

tfoot'__ :: Sig__ s opq
tfoot'__ x = PwElement "tfoot" dats [txt x]


th :: AFSig s opq
th x y = (\_ -> PwElement "th" x y)

th' :: Sig s opq
th' = PwElement "th"

th_ :: AFSig_ s opq
th_ x = (\_ -> PwElement "th" dats x)

th'_ :: Sig_ s opq
th'_ x = PwElement "th" dats x

th__ :: AFSig__ s opq
th__ x = (\_ -> PwElement "th" dats [txt x])

th'__ :: Sig__ s opq
th'__ x = PwElement "th" dats [txt x]


thead :: AFSig s opq
thead x y = (\_ -> PwElement "thead" x y)

thead' :: Sig s opq
thead' = PwElement "thead"

thead_ :: AFSig_ s opq
thead_ x = (\_ -> PwElement "thead" dats x)

thead'_ :: Sig_ s opq
thead'_ x = PwElement "thead" dats x

thead__ :: AFSig__ s opq
thead__ x = (\_ -> PwElement "thead" dats [txt x])

thead'__ :: Sig__ s opq
thead'__ x = PwElement "thead" dats [txt x]


time :: AFSig s opq
time x y = (\_ -> PwElement "time" x y)

time' :: Sig s opq
time' = PwElement "time"

time_ :: AFSig_ s opq
time_ x = (\_ -> PwElement "time" dats x)

time'_ :: Sig_ s opq
time'_ x = PwElement "time" dats x

time__ :: AFSig__ s opq
time__ x = (\_ -> PwElement "time" dats [txt x])

time'__ :: Sig__ s opq
time'__ x = PwElement "time" dats [txt x]


title :: AFSig s opq
title x y = (\_ -> PwElement "title" x y)

title' :: Sig s opq
title' = PwElement "title"

title_ :: AFSig_ s opq
title_ x = (\_ -> PwElement "title" dats x)

title'_ :: Sig_ s opq
title'_ x = PwElement "title" dats x

title__ :: AFSig__ s opq
title__ x = (\_ -> PwElement "title" dats [txt x])

title'__ :: Sig__ s opq
title'__ x = PwElement "title" dats [txt x]


tr :: AFSig s opq
tr x y = (\_ -> PwElement "tr" x y)

tr' :: Sig s opq
tr' = PwElement "tr"

tr_ :: AFSig_ s opq
tr_ x = (\_ -> PwElement "tr" dats x)

tr'_ :: Sig_ s opq
tr'_ x = PwElement "tr" dats x

tr__ :: AFSig__ s opq
tr__ x = (\_ -> PwElement "tr" dats [txt x])

tr'__ :: Sig__ s opq
tr'__ x = PwElement "tr" dats [txt x]


track :: AFSig s opq
track x y = (\_ -> PwElement "track" x y)

track' :: Sig s opq
track' = PwElement "track"

track_ :: AFSig_ s opq
track_ x = (\_ -> PwElement "track" dats x)

track'_ :: Sig_ s opq
track'_ x = PwElement "track" dats x

track__ :: AFSig__ s opq
track__ x = (\_ -> PwElement "track" dats [txt x])

track'__ :: Sig__ s opq
track'__ x = PwElement "track" dats [txt x]


tt :: AFSig s opq
tt x y = (\_ -> PwElement "tt" x y)

tt' :: Sig s opq
tt' = PwElement "tt"

tt_ :: AFSig_ s opq
tt_ x = (\_ -> PwElement "tt" dats x)

tt'_ :: Sig_ s opq
tt'_ x = PwElement "tt" dats x

tt__ :: AFSig__ s opq
tt__ x = (\_ -> PwElement "tt" dats [txt x])

tt'__ :: Sig__ s opq
tt'__ x = PwElement "tt" dats [txt x]


u :: AFSig s opq
u x y = (\_ -> PwElement "u" x y)

u' :: Sig s opq
u' = PwElement "u"

u_ :: AFSig_ s opq
u_ x = (\_ -> PwElement "u" dats x)

u'_ :: Sig_ s opq
u'_ x = PwElement "u" dats x

u__ :: AFSig__ s opq
u__ x = (\_ -> PwElement "u" dats [txt x])

u'__ :: Sig__ s opq
u'__ x = PwElement "u" dats [txt x]


ul :: AFSig s opq
ul x y = (\_ -> PwElement "ul" x y)

ul' :: Sig s opq
ul' = PwElement "ul"

ul_ :: AFSig_ s opq
ul_ x = (\_ -> PwElement "ul" dats x)

ul'_ :: Sig_ s opq
ul'_ x = PwElement "ul" dats x

ul__ :: AFSig__ s opq
ul__ x = (\_ -> PwElement "ul" dats [txt x])

ul'__ :: Sig__ s opq
ul'__ x = PwElement "ul" dats [txt x]


var :: AFSig s opq
var x y = (\_ -> PwElement "var" x y)

var' :: Sig s opq
var' = PwElement "var"

var_ :: AFSig_ s opq
var_ x = (\_ -> PwElement "var" dats x)

var'_ :: Sig_ s opq
var'_ x = PwElement "var" dats x

var__ :: AFSig__ s opq
var__ x = (\_ -> PwElement "var" dats [txt x])

var'__ :: Sig__ s opq
var'__ x = PwElement "var" dats [txt x]


video :: AFSig s opq
video x y = (\_ -> PwElement "video" x y)

video' :: Sig s opq
video' = PwElement "video"

video_ :: AFSig_ s opq
video_ x = (\_ -> PwElement "video" dats x)

video'_ :: Sig_ s opq
video'_ x = PwElement "video" dats x

video__ :: AFSig__ s opq
video__ x = (\_ -> PwElement "video" dats [txt x])

video'__ :: Sig__ s opq
video'__ x = PwElement "video" dats [txt x]


wbr :: (s -> PwNode s opq)
wbr = (\_ -> PwElement "br" dats [])

txt :: String -> (s -> PwNode s opq)
txt t = (\_ -> PwTextNode t)

txt' :: String -> PwNode s opq
txt' = PwTextNode


h1 :: AFSig s opq
h1 x y = (\_ -> PwElement "h1" x y)

h1' :: Sig s opq
h1' = PwElement "h1"

h1_ :: AFSig_ s opq
h1_ x = (\_ -> PwElement "h1" dats x)

h1'_ :: Sig_ s opq
h1'_ x = PwElement "h1" dats x

h1__ :: AFSig__ s opq
h1__ x = (\_ -> PwElement "h1" dats [txt x])

h1'__ :: Sig__ s opq
h1'__ x = PwElement "h1" dats [txt x]


h2 :: AFSig s opq
h2 x y = (\_ -> PwElement "h2" x y)

h2' :: Sig s opq
h2' = PwElement "h2"

h2_ :: AFSig_ s opq
h2_ x = (\_ -> PwElement "h2" dats x)

h2'_ :: Sig_ s opq
h2'_ x = PwElement "h2" dats x

h2__ :: AFSig__ s opq
h2__ x = (\_ -> PwElement "h2" dats [txt x])

h2'__ :: Sig__ s opq
h2'__ x = PwElement "h2" dats [txt x]


h3 :: AFSig s opq
h3 x y = (\_ -> PwElement "h3" x y)

h3' :: Sig s opq
h3' = PwElement "h3"

h3_ :: AFSig_ s opq
h3_ x = (\_ -> PwElement "h3" dats x)

h3'_ :: Sig_ s opq
h3'_ x = PwElement "h3" dats x

h3__ :: AFSig__ s opq
h3__ x = (\_ -> PwElement "h3" dats [txt x])

h3'__ :: Sig__ s opq
h3'__ x = PwElement "h3" dats [txt x]


h4 :: AFSig s opq
h4 x y = (\_ -> PwElement "h4" x y)

h4' :: Sig s opq
h4' = PwElement "h4"

h4_ :: AFSig_ s opq
h4_ x = (\_ -> PwElement "h4" dats x)

h4'_ :: Sig_ s opq
h4'_ x = PwElement "h4" dats x

h4__ :: AFSig__ s opq
h4__ x = (\_ -> PwElement "h4" dats [txt x])

h4'__ :: Sig__ s opq
h4'__ x = PwElement "h4" dats [txt x]


h5 :: AFSig s opq
h5 x y = (\_ -> PwElement "h5" x y)

h5' :: Sig s opq
h5' = PwElement "h5"

h5_ :: AFSig_ s opq
h5_ x = (\_ -> PwElement "h5" dats x)

h5'_ :: Sig_ s opq
h5'_ x = PwElement "h5" dats x

h5__ :: AFSig__ s opq
h5__ x = (\_ -> PwElement "h5" dats [txt x])

h5'__ :: Sig__ s opq
h5'__ x = PwElement "h5" dats [txt x]


h6 :: AFSig s opq
h6 x y = (\_ -> PwElement "h6" x y)

h6' :: Sig s opq
h6' = PwElement "h6"

h6_ :: AFSig_ s opq
h6_ x = (\_ -> PwElement "h6" dats x)

h6'_ :: Sig_ s opq
h6'_ x = PwElement "h6" dats x

h6__ :: AFSig__ s opq
h6__ x = (\_ -> PwElement "h6" dats [txt x])

h6'__ :: Sig__ s opq
h6'__ x = PwElement "h6" dats [txt x]
