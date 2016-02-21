#version 150

out vec4 outColor;
in float height;

void main()
{
    //if (height >= 0)
    //    discard;

    outColor = vec4(0.2, 0.2, 0.2, 1.0);
}
