package com.tolkflip.utils;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.tolkflip.models.Message;

import java.lang.reflect.Type;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class Converters {
    private static Gson gson = new Gson();

    @TypeConverter
    public static Date fromTimestamp(Long value) {
        return value == null ? null : new Date(value);
    }

    @TypeConverter
    public static Long dateToTimestamp(Date date) {
        return date == null ? null : date.getTime();
    }

    @TypeConverter
    public static String fromMessageType(Message.MessageType type) {
        return type == null ? null : type.name();
    }

    @TypeConverter
    public static Message.MessageType toMessageType(String value) {
        return value == null ? null : Message.MessageType.valueOf(value);
    }

    @TypeConverter
    public static String fromMessageStatus(Message.MessageStatus status) {
        return status == null ? null : status.name();
    }

    @TypeConverter
    public static Message.MessageStatus toMessageStatus(String value) {
        return value == null ? null : Message.MessageStatus.valueOf(value);
    }

    @TypeConverter
    public static String fromStringList(List<String> list) {
        return list == null ? null : gson.toJson(list);
    }

    @TypeConverter
    public static List<String> toStringList(String value) {
        if (value == null) return null;
        Type listType = new TypeToken<List<String>>() {}.getType();
        return gson.fromJson(value, listType);
    }

    @TypeConverter
    public static String fromStringMap(Map<String, String> map) {
        return map == null ? null : gson.toJson(map);
    }

    @TypeConverter
    public static Map<String, String> toStringMap(String value) {
        if (value == null) return null;
        Type mapType = new TypeToken<Map<String, String>>() {}.getType();
        return gson.fromJson(value, mapType);
    }

    @TypeConverter
    public static String fromByteArray(byte[] bytes) {
        return bytes == null ? null : android.util.Base64.encodeToString(bytes, android.util.Base64.DEFAULT);
    }

    @TypeConverter
    public static byte[] toByteArray(String value) {
        return value == null ? null : android.util.Base64.decode(value, android.util.Base64.DEFAULT);
    }
}
