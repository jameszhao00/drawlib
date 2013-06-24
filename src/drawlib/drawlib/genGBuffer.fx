struct VS_IN
{
	float3 pos : POSITION;
	float3 normal : NORMAL;
};

struct PS_IN
{
	float4 pos : SV_POSITION;
	float3 normal : NORMAL;
};

struct PS_OUT 
{
	float4 albedo : SV_TARGET0;
	float4 normal : SV_TARGET1;
};

float4x4 worldViewProj;

PS_IN VS( VS_IN input )
{
	PS_IN output = (PS_IN)0;	
	output.pos = mul(worldViewProj, float4(input.pos, 1));
	output.normal = input.normal;
	return output;
}

PS_OUT PS( PS_IN input ) 
{
	PS_OUT output = (PS_OUT)0;
	output.albedo = float4(input.normal, 1);
	output.normal = float4(input.normal, 1);
	return output;
}