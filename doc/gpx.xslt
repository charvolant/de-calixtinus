<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:gpx="http://www.topografix.com/GPX/1/1">
    <xsl:output method="text" />
    <xsl:template match="/">
latitude,longitude,elevation
        <xsl:apply-templates select="gpx:gpx"/>
    </xsl:template>
    <xsl:template match="gpx:gpx">
        <xsl:apply-templates select="gpx:trk"/>
    </xsl:template>
    <xsl:template match="gpx:trk">
        <xsl:apply-templates select="gpx:trkseg"/>
    </xsl:template>
    <xsl:template match="gpx:trkseg">
        <xsl:apply-templates select="gpx:trkpt"/>
    </xsl:template>
    <xsl:template match="gpx:trkpt">
<xsl:value-of select="@lat"/>,<xsl:value-of select="@lon"/>,<xsl:value-of select="gpx:ele"/><xsl:text>
</xsl:text>
    </xsl:template>
</xsl:stylesheet>

