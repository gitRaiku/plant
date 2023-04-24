#include <stdint.h>

static const double timeout = 2.2;

static const uint32_t barWidth  = 500;

static const uint32_t borderThickness = 1; // px
static const uint32_t hpad = 10; // Horizontal padding in px from the border
static const uint32_t vpad = 11; // Vertical padding in px from the border
static const uint32_t linepad = 3; // Padding between text lines

static const uint32_t topm = 80; // Distance from the top of the screen in px

static const uint64_t background_col = 0xFF3E3E3E; /// AARRGGBB
static const uint64_t foreground_col = 0xFFECECEC;
static const uint64_t     border_col = 0xFFECECEC;

static const char *fonts[2] = { "JetBrainsMono:size=18", // Written using fontconfig selectors
                                "Koruri:size=18" };      // Max 2 fonts, Leave NULL if you 
                                                         // don't want a fallback font
                                                         //
