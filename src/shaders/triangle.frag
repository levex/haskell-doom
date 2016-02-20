#version 150

out vec4 outColor;

in vec2 Texcoord;

uniform sampler2D tex;

void main()
{
    outColor = texture(tex, Texcoord);
}
