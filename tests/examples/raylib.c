#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct { uint8_t r, g, b, a; } Color;
typedef struct { float x, y; } Vector2;
typedef struct { void *data; int32_t width, height, mipmaps, format; } Image;
typedef struct { uint32_t id; int32_t width, height, mipmaps, format; } Texture2D;

static int close_checks, drawing, frames, images, textures, window_open;
static int life, resizable, image_calls, texture_calls, updates, draws;
static int random_calls, reference_random_calls, reference_width, reference_height;
static unsigned char *reference;
static uint32_t next_texture;
static int next_image_format = 7;
void FixtureNextImageFormat(int format) { next_image_format = format; }
static const Color gray = {80, 80, 80, 255};
static const Color white = {245, 245, 245, 255};
static const Color life_white = {255, 255, 255, 255};
static const int sizes[][2] = {
    {40, 32}, {56, 40}, {24, 16}, {24, 16}, {0, 0}, {7, 7}, {24, 16},
    {31, 23}, {8, 8}, {8, 24}, {24, 8}, {800, 600}
};
#define LIFE_FRAMES ((int)(sizeof sizes / sizeof sizes[0]))

static int failure(const char *stage) {
    const char *selected = getenv("CASA_RAYLIB_FAILURE");
    return selected && strcmp(selected, stage) == 0;
}

static int same_color(Color a, Color b) { return memcmp(&a, &b, sizeof a) == 0; }
static int random_value(int index) { return (index * 37 + 11) % 256; }

void InitWindow(int width, int height, const char *title) {
    assert(!window_open);
    life = strcmp(title, "Casa Game of Life") == 0;
    assert(width == 800 && height == (life ? 600 : 450));
    assert(life || strcmp(title, "Casa raylib example") == 0 || strcmp(title, "Casa") == 0);
    frames = close_checks = resizable = image_calls = texture_calls = updates = draws = 0;
    random_calls = reference_random_calls = 0;
    window_open = !failure("window");
    if (life && window_open) {
        reference_width = reference_height = 1;
        reference = malloc(1);
        assert(reference);
        reference[0] = random_value(reference_random_calls++) < 64;
    }
}

_Bool IsWindowReady(void) { return window_open; }
void SetWindowState(uint32_t flags) { assert(window_open && flags == 4); resizable = 1; }
void SetTargetFPS(int fps) { assert(window_open && fps == (life ? 5 : 60)); }
_Bool IsWindowMinimized(void) { assert(window_open); return frames == 3; }
int GetScreenWidth(void) { assert(window_open && resizable); return sizes[frames][0]; }
int GetScreenHeight(void) { assert(window_open && resizable); return sizes[frames][1]; }
int GetRandomValue(int minimum, int maximum) {
    assert(window_open && minimum == 0 && maximum == 255);
    return random_value(random_calls++);
}

Image ImageCopy(Image source) {
    assert(!drawing && source.width > 0 && source.height > 0);
    assert(source.mipmaps == 1 && source.format == 7);
    const Color *pixels = source.data;
    for (int i = 0; i < source.width * source.height; ++i)
        assert(same_color(pixels[i], life ? gray : white));
    image_calls++;
    if (failure("image") || (failure("resize_image") && image_calls == 3)) return (Image){0};
    size_t bytes = (size_t)source.width * source.height * sizeof(Color);
    void *copy = malloc(bytes);
    assert(copy);
    memcpy(copy, source.data, bytes);
    images++;
    int format = next_image_format;
    next_image_format = 7;
    return (Image){copy, source.width, source.height, 1, format};
}

_Bool IsImageValid(Image image) { return image.data != 0; }
Texture2D LoadTextureFromImage(Image image) {
    assert(window_open && images && !drawing && image.data);
    assert(image.mipmaps == 1 && image.format == 7);
    texture_calls++;
    if (failure("texture") || (failure("resize_texture") && texture_calls == 3)) return (Texture2D){0};
    textures++;
    return (Texture2D){++next_texture, image.width, image.height, 1, 7};
}
_Bool IsTextureValid(Texture2D texture) { return texture.id != 0; }
void UnloadImage(Image image) {
    if (!image.data) { assert(failure("image") || failure("resize_image")); return; }
    assert(images > 0);
    free(image.data);
    images--;
}
void ImageClearBackground(Image *image, Color color) {
    assert(image->data && same_color(color, gray));
    Color *pixels = image->data;
    for (int i = 0; i < image->width * image->height; ++i) pixels[i] = color;
}
void ImageDrawPixel(Image *image, int x, int y, Color color) {
    assert(image->data && x >= 0 && x < image->width && y >= 0 && y < image->height);
    assert(same_color(color, life ? life_white : white));
    ((Color *)image->data)[y * image->width + x] = color;
}

_Bool WindowShouldClose(void) {
    assert(window_open && !drawing && textures == 1);
    assert(images == (life ? 1 : 0));
    return close_checks++ >= (life ? LIFE_FRAMES : 2);
}
Vector2 GetMousePosition(void) {
    assert(window_open);
    if (!life) return (Vector2){12.5f, 25.0f};
    if (frames == 6) return (Vector2){-0.5f, 1.0f};
    if (frames == 7) return (Vector2){30.0f, 22.0f}; /* Unused edge pixels. */
    if (frames == 9) return (Vector2){0.0f, 1000.0f};
    return (Vector2){0.0f, 0.0f};
}
_Bool IsMouseButtonDown(int button) {
    assert(window_open && (button == 0 || button == 1));
    return button == 0 ? frames != 2 && frames != 11 : frames == 1 || frames == 2;
}

static void advance_reference(int width, int height) {
    unsigned char *resized = calloc((size_t)width * height, 1);
    unsigned char *next = calloc((size_t)width * height, 1);
    assert(resized && next);
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            resized[y * width + x] = x < reference_width && y < reference_height
                ? reference[y * reference_width + x]
                : random_value(reference_random_calls++) < 64;
        }
    }
    assert(random_calls == reference_random_calls);
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            int neighbors = 0;
            for (int dy = -1; dy <= 1; ++dy)
                for (int dx = -1; dx <= 1; ++dx)
                    if (dx || dy) neighbors += resized[((y + dy + height) % height) * width + (x + dx + width) % width];
            next[y * width + x] = neighbors == 3 || (neighbors == 2 && resized[y * width + x]);
        }
    }
    Vector2 mouse = GetMousePosition();
    if ((IsMouseButtonDown(0) || IsMouseButtonDown(1)) &&
        mouse.x >= 0 && mouse.y >= 0 && mouse.x < width * 8 && mouse.y < height * 8)
        next[(int)(mouse.y / 8) * width + (int)(mouse.x / 8)] = !IsMouseButtonDown(1);
    free(resized);
    free(reference);
    reference = next;
    reference_width = width;
    reference_height = height;
}

void UpdateTexture(Texture2D texture, const void *data) {
    assert(window_open && !drawing && textures && data);
    assert(texture.mipmaps == 1 && texture.format == 7);
    if (life) {
        int width = sizes[frames][0] / 8, height = sizes[frames][1] / 8;
        assert(width > 0 && height > 0 && texture.width == width && texture.height == height);
        advance_reference(width, height);
        const Color *pixels = data;
        for (int i = 0; i < width * height; ++i)
            assert(same_color(pixels[i], reference[i] ? life_white : gray));
    } else {
        assert(texture.width == 64 && texture.height == 48);
        const Color *pixels = data;
        for (int i = 0; i < 64 * 48; ++i)
            assert(same_color(pixels[i], i == 64 * 48 - 1 ? white : gray));
    }
    updates++;
}
void BeginDrawing(void) { assert(window_open && !drawing); drawing = 1; }
void ClearBackground(Color color) { assert(drawing && same_color(color, gray)); }
void DrawTextureV(Texture2D texture, Vector2 position, Color tint) {
    assert(drawing && textures && texture.id);
    assert(texture.width == 64 && texture.height == 48 && texture.mipmaps == 1 && texture.format == 7);
    assert(position.x == 12.5f && position.y == 25.0f && same_color(tint, white));
}
void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint) {
    assert(drawing && textures && texture.id);
    assert(position.x == 0 && position.y == 0 && rotation == 0 && scale == 8 && same_color(tint, life ? life_white : white));
    if (life) {
        assert(texture.width == sizes[frames][0] / 8 && texture.height == sizes[frames][1] / 8);
        assert(updates == draws + 1);
    }
    draws++;
}
void EndDrawing(void) { assert(drawing && textures); drawing = 0; frames++; }
void UnloadTexture(Texture2D texture) {
    assert(window_open && !drawing);
    if (!texture.id) { assert(failure("texture") || failure("resize_texture")); return; }
    assert(textures > 0);
    textures--;
}
void CloseWindow(void) {
    assert(window_open && !images && !textures && !drawing);
    window_open = 0;
    if (!getenv("CASA_RAYLIB_FAILURE")) {
        assert(frames == (life ? LIFE_FRAMES : 2));
        if (life) assert(updates == LIFE_FRAMES - 3 && draws == updates);
        else assert(updates == draws && updates <= 1);
    }
    free(reference);
    reference = NULL;
    assert(write(STDOUT_FILENO, "raylib stub ok\n", 15) == 15);
}
