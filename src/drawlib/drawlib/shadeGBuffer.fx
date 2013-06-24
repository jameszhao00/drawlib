struct VS_IN
{
	float3 pos : POSITION;
};

struct PS_IN
{
	float4 pos : SV_POSITION;
};

struct PS_OUT 
{
	float4 color : SV_TARGET;
};

Texture2D<float4> albedo : register(t0); 
Texture2D<float4> normal : register(t1);

PS_IN VS( VS_IN input )
{
	PS_IN output = (PS_IN)0;	
	output.pos = float4(input.pos, 1);
	return output;
}

PS_OUT PS( PS_IN input )
{
	PS_OUT output = (PS_OUT)0;
	output.color = normal.Load(int3(input.pos.x, input.pos.y, 0));
	return output;
}