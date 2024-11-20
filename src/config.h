#include <stdint.h>

static const double timeout = 2.2;

static const uint32_t barWidth  = 500;

static const uint32_t borderThickness = 2; // px
static const uint32_t hpad = 10; // Horizontal padding in px from the border
static const uint32_t vpad = 10; // Vertical padding in px from the border
static const uint32_t linepad = 3; // Padding between text lines

static const double gammaCorrection = 0.6; // Gamma correction coefficient for the font
                                           // Why is this less than 1 and why does
                                           // Freetype say 1.8 is good

static const uint32_t posx = 30; // Distance from the right of the screen in px
static const uint32_t posy = 60; // Distance from the top of the screen in px

static const uint64_t background_col = 0xFF3E3E3E; /// AARRGGBB
static const uint64_t foreground_col = 0xFFECECEC; /// Transparency is supported but wayland has
static const uint64_t     border_col = 0xFFECECEC; /// This problem where if #00FFFFFF is not 
                                                   /// Fully transparent, so you cannot get true
                                                   /// Transparency.

static const char *fonts[2] = { "JetBrainsMono:size=20:style=Normal", // Written using fontconfig selectors
                                "Koruri:size=19" };      // Max 2 fonts, Leave NULL if you 
                                                         // don't want a fallback font
