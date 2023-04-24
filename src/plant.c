#include "plant.h"

VECTOR_SUITE(seat, struct cseat)
VECTOR_SUITE(wch, wchar_t *)

struct fontInfo {
  char *fname;
  FT_F26Dot6 xsize, ysize;  /* pixel size */
  FcBool     aa;            /* doing antialiasing */
  FcBool     embold;        /* force emboldening */
  FcBool     color;         /* contains color glyphs */
  int        rgba;          /* subpixel order */
  int        lcd_filter;    /* lcd filter */
  FcBool     hinting;       /* non-identify matrix? */
  FT_Int     load_flags;    /* glyph load flags */

  /*
   * Internal fields
   */
  int      spacing;
  FcBool    minspace;
  int      cwidth;
};

struct cfont {
  FT_Library l;
  FT_Face face[2];
  struct fontInfo i[2];
};
struct cfont fts = {0};

struct cstate {
  struct wl_display *dpy;
  struct wl_registry *reg;
  struct wl_shm *shm;
  struct wl_compositor *comp;
  struct xdg_wm_base *xwmBase;
  struct wl_region *oreg;
  struct xdg_surface *xsurf;
  struct xdg_toplevel *xtlev;
  struct zwlr_layer_shell_v1 *zwlr;
  struct zxdg_output_manager_v1 *xoutmgr;
  struct seatv seats;
  //struct outpv outs;
  struct wl_cursor_image *pImg;
  struct wl_surface *pSurf;
  struct cmon mon;

  uint32_t width, height;
  uint8_t closed;
  uint32_t tlen[9]; // Length in px of every tag icon
};
struct cstate state = {0};
uint8_t SHOULD_CLOSE = 0;
uint32_t barHeight = 10;
struct wchv lins;

// This section is here only because wayland refuses to allow NULL listeners, smh
// https://gitlab.freedesktop.org/wayland/wayland/-/issues/160                   
void zxout_logical_position(void *d, struct zxdg_output_v1 *z, int x, int y) { }
void zxout_logical_size(void *d, struct zxdg_output_v1 *z, int x, int y) { }
void zxout_done(void *d, struct zxdg_output_v1 *z) { }
void zxout_description(void *d, struct zxdg_output_v1 *z, const char *c) { }
void seat_name(void *d, struct wl_seat *s, const char *n) { }
void p_enter(void *data, struct wl_pointer *ptr, uint32_t serial, struct wl_surface *surface, wl_fixed_t x, wl_fixed_t y) {}
void p_leave(void *data, struct wl_pointer *ptr, uint32_t serial, struct wl_surface *surf) {}
void p_motion(void *data, struct wl_pointer *ptr, uint32_t time, wl_fixed_t x, wl_fixed_t y) {}
void p_axis(void *d, struct wl_pointer *p, uint32_t t, uint32_t a, wl_fixed_t v) { }
void p_axis_source(void *d, struct wl_pointer *p, uint32_t s) { }
void p_axis_stop(void *d, struct wl_pointer *p, uint32_t t, uint32_t s) { }
void p_axis_discrete(void *d, struct wl_pointer *p, uint32_t t, int s) { }
void reg_global_remove(void *data, struct wl_registry *reg, uint32_t name) { }

// Now onto the actual (shit) code

void xwmb_ping(void *data, struct xdg_wm_base *xdg_wm_base, uint32_t serial) {
    xdg_wm_base_pong(xdg_wm_base, serial);
}
const struct xdg_wm_base_listener xwmb_listener = { .ping = xwmb_ping };

void p_button(void *data, struct wl_pointer *ptr, uint32_t serial, uint32_t time, uint32_t button, uint32_t pressed) {
  struct cseat *seat = data;
  seat->p.cpres = (pressed == WL_POINTER_BUTTON_STATE_PRESSED);
}

void p_frame(void *data, struct wl_pointer *ptr) {
  struct cseat *seat = data;
  if (seat->p.cpres) {
    SHOULD_CLOSE = 1;
  }
}

const struct wl_pointer_listener pointer_listener = { .enter = p_enter, .leave = p_leave, .motion = p_motion, .button = p_button, .frame = p_frame, .axis = p_axis, .axis_source = p_axis_source, .axis_stop = p_axis_stop, .axis_discrete = p_axis_discrete };

void seat_caps(void *data, struct wl_seat *s, uint32_t caps) {
  struct cseat *seat = data;
  uint8_t hasPointer = caps & WL_SEAT_CAPABILITY_POINTER;
  if (!seat->p.p && hasPointer) {
    seat->p.p = wl_seat_get_pointer(seat->s);
    wl_pointer_add_listener(seat->p.p, &pointer_listener, seat);
  }
}
const struct wl_seat_listener seat_listener = { .capabilities = seat_caps, .name = seat_name };

void zxout_name(void *data, struct zxdg_output_v1 *xout, const char* name) {
  struct cmon *mon = data;
  mon->xdgname = name;
  zxdg_output_v1_destroy(xout);
}
const struct zxdg_output_v1_listener zxout_listener = { .name = zxout_name, .logical_position = zxout_logical_position, .logical_size = zxout_logical_size, .done = zxout_done, .description = zxout_description };

void init_rand() {
  FILE *__restrict sr = fopen("/dev/urandom", "r");
  int32_t sd = 0;
  int32_t i;
  for(i = 0; i < 4; ++i) {
    sd = (sd << 8) + fgetc(sr);
  }
  srand(fgetc(sr));
  fclose(sr);
}

void gname(char *__restrict s) {
  uint32_t fnl = strlen(s);
  int32_t i;
  uint32_t r;
  for(i = 1; i <= 6; ++i) {
    r = random();
    s[fnl - i] = 'A' + (r & 15) + ((r & 16) << 1);
  }
}

int32_t cshmf(uint32_t size) {
  char fnm[] = "plant-000000";

  int32_t fd = 0;
  uint32_t retries = 100;
  do {
    gname(fnm);
    fd = shm_open(fnm, O_RDWR | O_CREAT | O_EXCL, 0600);
    if (fd >= 0) {
      break;
    }
  } while (--retries);
  if (retries == 0) {
    exit(1);
  }
  WLCHECKE(!shm_unlink(fnm),"Could not unlink the shm file!");
  WLCHECKE(!ftruncate(fd, size),"Could not truncate the shm file!");
  return fd;
}

void cbufs(struct cmon *mon, uint32_t width, uint32_t height, enum wl_shm_format form) {
  uint32_t stride = width * 4;
  uint32_t size = stride * height;
  uint32_t fd = cshmf(size);
  struct wl_shm_pool *pool = wl_shm_create_pool(state.shm, fd, size);
  uint32_t *data = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  WLCHECK(data!=MAP_FAILED,"Could not mmap the shm data!");
  mon->sb.b = wl_shm_pool_create_buffer(pool, 0, width, height, stride, form);
  WLCHECK(mon->sb.b,"Could not create the first shm buffer!");
  wl_shm_pool_destroy(pool);

  mon->sb.fd = fd;
  mon->sb.data = data;
  mon->sb.size = size;
  mon->sb.width = width;
  mon->sb.height = height;
}

uint8_t lerp(uint8_t t, uint8_t o1, uint8_t o2) {
  return (uint8_t)((double)o1 * (1.0 - ((double)t/255.0))) + (o2 * ((double)t/255.0));
}

uint64_t ablend(uint8_t t, uint64_t fc, uint64_t bc) { /// TODO: Actual alpha blending
#define CTG(a,b,c,d) {b=((a&0xFF0000)>>16);c=((a&0x00FF00)>>8);d=((a&0x0000FF)>>0);}
  uint8_t r1, r2, r3, g1, g2, g3, b1, b2, b3;
  CTG(fc, r1, g1, b1);
  CTG(bc, r2, g2, b2);
  r3 = lerp(255 - t, r1, r2);
  g3 = lerp(255 - t, g1, g2);
  b3 = lerp(255 - t, b1, b2);
  return 0xFF000000 |
        ((uint64_t)r3 << 16) |
        ((uint64_t)g3 <<  8) |
        ((uint64_t)b3 <<  0);
#undef CTG
}

void draw_char(struct cmon *mon, int32_t x, int32_t y, uint8_t cs, uint64_t fc, uint64_t bc) {
#define CG fts.face[cs]->glyph
#define CB fts.face[cs]->glyph->bitmap
#define CM fts.face[cs]->glyph->metrics
  int32_t i, j;
  for (i = 0; i < CB.rows; ++i) {
    if ((i + y) < 0 || mon->sb.height <= (i + y)) {
      continue;
    }
    for (j = 0; j < CB.width; ++j) {
      if ((j + x) < 0 || mon->sb.width <= (j + x)) {
        continue;
      }
      mon->sb.data[(i + y) * mon->sb.width + j + x] = ablend(CB.buffer[i * CB.pitch + j], fc, bc);
    }
  }
}

void draw_rect(struct cmon *mon, uint32_t x, uint32_t y, int32_t w, int32_t h, uint64_t col) {
  int32_t i, j;
  for (i = 0; i < h; ++i) {
    if ((i + y) < 0 || mon->sb.height <= (i + y)) {
      continue;
    }
    for (j = 0; j < w; ++j) {
      if ((j + x) < 0 || mon->sb.width <= (j + x)) {
        continue;
      }
      mon->sb.data[(i + y) * mon->sb.width + j + x] = col;
    }
  }
}

int32_t draw_string(struct cmon *mon, const wchar_t *__restrict s, uint32_t x, uint32_t y, uint64_t fc, uint64_t bc, uint8_t render, uint8_t *__restrict used) { /// TODO: LCD filter
  int32_t i;
  int32_t px = 0;
  uint32_t cg;
  uint8_t cs = 0;
  uint32_t strl = wcslen(s);
  uint8_t pvt = 0;
  uint32_t pv = 0;
  FT_Vector kerning;
  if (used) {
    *used = 0;
  }

  for(i = 0; i < strl; ++i) {
    cg = FT_Get_Char_Index(fts.face[0], s[i]); cs = 0;
    if (cg == 0) { cg = FT_Get_Char_Index(fts.face[1], s[i]); cs = 1; }
    if (used) {
      *used |= (cs + 1);
    }
    FTCHECK(FT_Load_Glyph(fts.face[cs], cg, fts.i[cs].load_flags), "Could not load a glyph!");
    FTCHECK(FT_Render_Glyph(CG, FT_RENDER_MODE_NORMAL), "Could not render a glyph!");

    if (render) {
      int32_t xoff =   CM.horiBearingX >> 6;
      int32_t yoff = -(CM.horiBearingY >> 6);
      draw_char(mon, x + px + xoff, y + yoff, cs, fc, bc);
    }

    if (i > 0 && (pvt == cs) && FT_HAS_KERNING(fts.face[cs])) {
      FTCHECK(FT_Get_Kerning(fts.face[cs], pv, cg, FT_KERNING_DEFAULT, &kerning),"Could not get kerning for the given fonts");
      px += kerning.x >> 6;
    } else {
      px += CM.horiAdvance >> 6;
    }
    pvt = cs;
    pv = cg;
  }
#undef CG
#undef CM
  return px;
}

int32_t __inline__ __attribute((pure)) max(int32_t o1, int32_t o2) {
  return o1 > o2 ? o1 : o2;
}

void render(struct cmon *mon, wchar_t *__restrict *__restrict lines, uint32_t lc) {
  if (!mon->sb.b && lc) { return; }

  //fprintf(stdout, "T: %u\n", lc);

  draw_rect(mon, 0, 0, barWidth    , barHeight    ,     border_col); /// Very efficient, i know
  draw_rect(mon, borderThickness, borderThickness, barWidth - borderThickness * 2, barHeight - borderThickness * 2, background_col);
  
  {
    int32_t i;
    uint8_t used;
    int32_t vt = 0;
    int32_t py;
    for(i = 0; i < lc; ++i) {
      draw_string(mon, lines[i], 0, 0, 0, 0, 0, &used);
      py = (fts.face[0]->height + fts.face[0]->descender) >> 6;
      draw_string(mon, lines[i], borderThickness + hpad, borderThickness + vpad + py + vt, foreground_col, background_col, 1, &used);
      py = 0;
      py = (fts.face[0]->height >> 6) + linepad;
      vt += py;
      fprintf(stdout, "%ls -> %u\n", lines[i], vt);
    }
  }

  wl_surface_attach(mon->surf, mon->sb.b, 0, 0);
  wl_surface_damage(mon->surf, 0, 0, barWidth, barHeight);
  wl_surface_commit(mon->surf);
}

void zwlr_configure(void *data, struct zwlr_layer_surface_v1 *l, uint32_t serial, uint32_t width, uint32_t height) { 
  struct cmon *mon = data;
  zwlr_layer_surface_v1_ack_configure(l, serial);
  if (mon->sb.b && width == mon->sb.width && height == mon->sb.height) { return; }
  cbufs(mon, width, height, WL_SHM_FORMAT_ARGB8888);
}
void zwlr_closed(void *data, struct zwlr_layer_surface_v1 *l) { }
struct zwlr_layer_surface_v1_listener zwlr_listener = { .configure = zwlr_configure, .closed = zwlr_closed };

void finish_init() {
#define CHECK_INIT(x, e, v) {if (!state. x) { fprintf(stderr, "Your wayland compositor does not support " #e " version " #v "which is required for me to work :(\n"); exit(1); }}
  CHECK_INIT(comp      , wl_compositor         , COMPV   );
  CHECK_INIT(shm       , wl_shm                , SHMV    );
  CHECK_INIT(zwlr      , zwlr_layer_shell_v1   , ZWLRV   );
  CHECK_INIT(xoutmgr   , zxdg_output_manager_v1, XOUTMGRV);
#undef CHECK_INIT

  seatvt(&state.seats);
  state.mon.xout = zxdg_output_manager_v1_get_xdg_output(state.xoutmgr, state.mon.out);
  zxdg_output_v1_add_listener(state.mon.xout, &zxout_listener, &state.mon.out);
  wl_display_roundtrip(state.dpy);
}

void reg_global(void *data, struct wl_registry *reg, uint32_t name, const char *iface, uint32_t ver) {
#define CHI(x,y,z,w) {if(!strcmp(iface,y .name)) {state. x=wl_registry_bind(reg, name, &y, z);w;return;}}
#define CHV(x,y,z,w) {if(!strcmp(iface,y .name)) {x cbind=wl_registry_bind(reg, name, &y, z);w;return;}}
  CHI(comp           , wl_compositor_interface         , COMPV,);
  CHI(shm            , wl_shm_interface                , SHMV,);
  CHI(zwlr           , zwlr_layer_shell_v1_interface   , ZWLRV,);
  CHI(xwmBase        , xdg_wm_base_interface           , XWMBASEV,
      xdg_wm_base_add_listener(state.xwmBase, &xwmb_listener, NULL));
  CHI(xoutmgr        , zxdg_output_manager_v1_interface, XOUTMGRV,);
  CHV(struct wl_seat*, wl_seat_interface               , SEATV,
      struct cseat cs = {0};
      cs.n = name;
      cs.s = cbind;
      seatvp(&state.seats, cs);
      wl_seat_add_listener(cbind, &seat_listener, state.seats.v + state.seats.l - 1););
  CHV(struct wl_output*, wl_output_interface           , WOUTV,
      /// TODO: Handle multiple monitors
      struct cmon mon = {0};
      mon.n = name;
      mon.out = cbind;
      state.mon = mon;);
#undef CHI
#undef CHV
}
const struct wl_registry_listener reg_listener = { .global = reg_global, .global_remove = reg_global_remove};

uint8_t get_font_info(FcPattern *pattern, struct fontInfo *__restrict i) {
#define QD(v,p,d) {switch (FcPatternGetDouble (pattern, p, 0, &v)) { case FcResultNoMatch: v = d; break; case FcResultMatch: break; default: goto fi_crash; }}
#define QB(v,p,d) {switch (FcPatternGetBool   (pattern, p, 0, &v)) { case FcResultNoMatch: v = d; break; case FcResultMatch: break; default: goto fi_crash; }}
#define QI(v,p,d) {switch (FcPatternGetInteger(pattern, p, 0, &v)) { case FcResultNoMatch: v = d; break; case FcResultMatch: break; default: goto fi_crash; }}
  char *cf;
  WLCHECK(FcPatternGetString(pattern, FC_FILE, 0, (FcChar8 **)&cf)==FcResultMatch,"Could not find a suitable font!");
  i->fname = malloc(strlen(cf));
  strcpy(i->fname, cf);
  
  double psize;
  double aspect;
  FcBool vl, hinting, ahint, gadv;
  int hstyle;

  WLCHECK(FcPatternGetDouble(pattern, FC_PIXEL_SIZE, 0, &psize)==FcResultMatch,"Could not get font pixel size!");
  QB(i->aa        , FC_ANTIALIAS      , FcTrue         );
  QI(i->rgba      , FC_RGBA           , FC_RGBA_UNKNOWN);
  QI(i->lcd_filter, FC_LCD_FILTER     , FC_LCD_LIGHT   );
  QB(i->embold    , FC_EMBOLDEN       , FcFalse        );
  QI(i->spacing   , FC_SPACING        , FC_PROPORTIONAL);
  QB(i->minspace  , FC_MINSPACE       , FcFalse        );
  QI(i->cwidth    , FC_CHAR_WIDTH     , 0              );
  QD(aspect       , FC_ASPECT         , 1.0            );
  QB(hinting      , FC_HINTING        , FcTrue         );
  QI(hstyle       , FC_HINT_STYLE     , FC_HINT_SLIGHT );
  QB(vl           , FC_VERTICAL_LAYOUT, FcFalse        );
  QB(ahint        , FC_AUTOHINT       , FcFalse        );
  QB(gadv         , FC_GLOBAL_ADVANCE , FcTrue         );
#undef QD
#undef QB
#undef QI

  i->ysize = psize * 64.0;
  i->xsize = psize * aspect * 64.0;

  i->load_flags = FT_LOAD_DEFAULT | FT_LOAD_COLOR;

  if (i->aa) { /// Code i defo didn't steal from libXft and i fully understand
    if (FC_HINT_NONE < hstyle && hstyle < FC_HINT_FULL) {
      i->load_flags |= FT_LOAD_TARGET_LIGHT;
    } else {
      switch (i->rgba) {
      case FC_RGBA_RGB:
      case FC_RGBA_BGR:
        i->load_flags |= FT_LOAD_TARGET_LCD;
        break;
      case FC_RGBA_VRGB:
      case FC_RGBA_VBGR:
        i->load_flags |= FT_LOAD_TARGET_LCD_V;
        break;
      }
    }
  } else { i->load_flags |= FT_LOAD_TARGET_MONO; }

  if (!hinting || hstyle == FC_HINT_NONE) { i->load_flags |= FT_LOAD_NO_HINTING; }
  if                                 (vl) { i->load_flags |= FT_LOAD_VERTICAL_LAYOUT; }
  if                              (ahint) { i->load_flags |= FT_LOAD_FORCE_AUTOHINT; }
  if                              (!gadv) { i->load_flags |= FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH; }
  if                       (i->cwidth) { i->spacing = FC_MONO; }

  return 0;
fi_crash:
  return 1;
}

void print_font_info(struct fontInfo *__restrict i) { fprintf(stdout, "Got font: %s\n\tPsize: %lix%li\n\tAntialias: %u\n\tEmbold: %u\n\tColor: %u\n\tRgba: %u\n\tLcdFilter: %u\n\tHinting: %u\n\tLoadFlags: 0x%x\n\tSpacing: %x\n\tMinspace: %x\n\tChar Width: %x\n", i->fname, i->xsize, i->ysize, i->aa, i->embold, i->color, i->rgba, i->lcd_filter, i->hinting, i->load_flags, i->spacing, i->minspace, i->cwidth); }

void find_font_face(const char *fname, FT_Face *face, struct fontInfo *i) {
  FcInit(); /// Boy do i love fontconfig and all their incredible documentation
  FcResult result;
  FcConfig *config = FcInitLoadConfigAndFonts();
  FcPattern *pat = FcNameParse((const FcChar8*)fname);
  FcConfigSubstitute(config, pat, FcMatchPattern);
  FcDefaultSubstitute(pat);
  FcPattern *font = FcFontMatch(config, pat, &result);
  WLCHECK(!get_font_info(font, i),"Could not get font information about the selected font!");
  FcPatternDestroy(font);
  FcPatternDestroy(pat);
  FcConfigDestroy(config);
  FcFini();

  //print_font_info(i);
  FTCHECK(FT_New_Face(fts.l, i->fname, 0, face),"Could not create a font face from the given pattern!");
  FTCHECK(FT_Set_Char_Size(*face, 0, 16 * 64, 300, 300),"Could not set the character size!");
  FTCHECK(FT_Set_Pixel_Sizes(*face, i->xsize >> 6, i->ysize >> 6),"Could not set the pixel size!");
}

void init_freetype() {
  FTCHECK(FT_Init_FreeType(&fts.l),"Could not initialise the Freetype lib!");
  find_font_face(fonts[0], fts.face, fts.i);
  if (fonts[1]) {
    find_font_face(fonts[1], fts.face + 1, fts.i + 1);
  }
}

uint32_t runel(char *__restrict str) {
  char *__restrict os = str;
  for (++str; (*str & 0xc0) == 0x80; ++str);
  return (uint32_t) (str - os);
}

uint32_t utf8_to_unicode(char *__restrict str, uint32_t l) {
  uint32_t res = 0;
  switch (l) {
    case 4:
      res |= *str & 0x7;
      break;
    case 3:
      res |= *str & 0xF;
      break;
    case 2:
      res |= *str & 0x1F;
      break;
    case 1:
      res |= *str & 0x7F;
      break;
  }

  --l;
  while (l) {
    ++str;
    res <<= 6;
    res |= *str & 0x3F;
    --l;
  }

  return res;
}

uint32_t utf2wwch(char *__restrict s, wchar_t *__restrict t) {
  uint32_t tl = 0;
  uint32_t cl;
  while (*s) {
    cl = runel(s);
    t[tl] = utf8_to_unicode(s, cl);
    if (t[tl] == L'\n' || t[tl] == L'\r') { --tl; }
    ++tl;
    s += cl;
  }
  t[tl] = L'\0';
  return tl;
}

void ts(double t) {
  uint32_t ft = floor(t);
  struct timespec tspec = { .tv_sec = ft, .tv_nsec = floor((t - ft) * 1000000000) };
  WLCHECKE(nanosleep(&tspec, NULL)==0, "");
}

void breakup(wchar_t *__restrict s, uint32_t sl) {
  //fprintf(stdout, "Got to break up %ls\n", s);
  int32_t maxl = barWidth - borderThickness * 2 - hpad * 2;
  {
    int32_t i, j;
    wchar_t a[1024];
    uint32_t cl  = 0; // Current length
    uint32_t cs  = 0; // Last separator
    uint32_t ca  = 0; // A length
    uint32_t cas = 0; // A length
    wchar_t  sep = 0; // Cur separator
    wchar_t *__restrict p;
    for(i = 0; i < sl; ++i) {
      while (s[i] != L' ' && s[i] != L'　' && s[i] != L'\n') {
        a[ca] = s[i];
        ++i;
        ++ca;
      }
      sep = s[i];
      a[ca] = L'\0';

      cl = draw_string(&state.mon, a, 0, 0, 0, 0, 0, NULL);
      //fprintf(stdout, "Added new word %u[%ls] len %u/%u\n", ca, a, cl, maxl);
      if (cl > maxl) {
        if (ca == 1) {
          //fprintf(stdout, "Character 2 big %lc i ain't botherin'", a[0]);
          continue;
        }
        if (cs == 0) {
          j = 1;
          sep = a[j];
          a[j] = L'\0';
          while (draw_string(&state.mon, a, 0, 0, 0, 0, 0, NULL) < maxl) {
            a[j] = sep;
            ++j;
            sep = a[j];
            a[j] = L'\0';
          }

          p = malloc(j * sizeof(p[0]) + 1);
          wcsncpy(p, a, j);
          p[j] = L'\0';
          wchvp(&lins, p);

          //fprintf(stdout, "Mid-splitting at %u got [%ls]\n", j, p);
          i = cas + j;
          cas = i;
          cs = 0;
          ca = 0;
        } else {
          p = malloc(cs * sizeof(p[0]) + 1);
          wcsncpy(p, a, cs);
          p[cs] = L'\0';
          wchvp(&lins, p);

          //fprintf(stdout, "Splitting at word boundary %u got [%ls]\n", ca, p);

          i = cas;
          cs = 0;
          ca = 0;
        }
      } else {
        a[ca] = sep;
        cs = ca;
        cas = i;
        ++ca;
      }
    }
    if (ca) {
      p = malloc(ca * sizeof(p[0]) + 1);
      wcsncpy(p, a, ca);
      p[ca] = L'\0';
      wchvp(&lins, p);
      //fprintf(stdout, "No more splitting %u got [%ls]\n", ca, p);
    }
  }
}

int main(int argc, char *argv[]) {
  setlocale(LC_ALL, "");
  if (argc < 2) {
    fputs("Usage:\n\tplant <text to display>\n", stderr);
    return 1;
  }

  memset(&state, 0, sizeof(state));
  init_rand();
  init_freetype();

  //wchar_t *__restrict *__restrict t;
  wchvi(&lins);
  {
    wchar_t txt[1024];
    int32_t i;
    uint32_t ct = 0;
    for(i = 1; i < argc; ++i) {
      ct = utf2wwch(argv[i], txt);
      txt[ct] = L' ';
      breakup(txt, ct);
    }
  }
  wchvt(&lins);

  barHeight = ((fts.face[0]->height >> 6) + linepad) * lins.l - linepad + borderThickness * 2 + vpad * 2;

  WLCHECK(state.dpy=wl_display_connect(NULL),"Could not connect to the wayland display!");
  WLCHECK(state.reg=wl_display_get_registry(state.dpy),"Could not fetch the wayland registry!");
  seatvi(&state.seats);
  wl_registry_add_listener(state.reg, &reg_listener, NULL);
  wl_display_roundtrip(state.dpy);
  finish_init();

  state.mon.surf = wl_compositor_create_surface(state.comp); WLCHECK(state.mon.surf,"Cannot create wayland surface!");
  state.mon.lsurf = zwlr_layer_shell_v1_get_layer_surface(state.zwlr, state.mon.surf, state.mon.out, ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY, "Plant"); WLCHECK(state.mon.lsurf,"Cannot create zwlr surface!");
  zwlr_layer_surface_v1_add_listener(state.mon.lsurf, &zwlr_listener, &state.mon);
  zwlr_layer_surface_v1_set_anchor(state.mon.lsurf, ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT | ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP);
  zwlr_layer_surface_v1_set_margin(state.mon.lsurf, topm, 0, 0, 0);

  zwlr_layer_surface_v1_set_size(state.mon.lsurf, barWidth, barHeight);
  zwlr_layer_surface_v1_set_keyboard_interactivity(state.mon.lsurf, ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE);
  wl_surface_commit(state.mon.surf);
  wl_display_dispatch(state.dpy);

  render(&state.mon, lins.v, lins.l);
  wl_display_dispatch(state.dpy);
  ts(timeout);

  return 0;
}
