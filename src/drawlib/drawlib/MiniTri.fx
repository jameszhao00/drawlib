struct VS_IN
{
	float3 pos : POSITION;
	float3 normal : NORMAL;
	//float4 col : COLOR;
};

struct PS_IN
{
	float4 pos : SV_POSITION;
	float3 normal : NORMAL;
};

float4x4 worldViewProj;

PS_IN VS( VS_IN input )
{
	PS_IN output = (PS_IN)0;
	
	output.pos = mul(worldViewProj, float4(input.pos, 1));
	output.normal = input.normal;
	return output;
}

float4 PS( PS_IN input ) : SV_Target
{
	return float4(input.normal, 1);
}