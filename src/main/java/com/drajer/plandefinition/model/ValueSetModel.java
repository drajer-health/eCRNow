package com.drajer.plandefinition.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.TypeDefs;
import org.hibernate.annotations.UpdateTimestamp;

import com.drajer.plandefinition.config.JSONObjectUserType;


@Entity
@Table(name = "valueset")
@TypeDefs({ @TypeDef(name = "StringJsonObject", typeClass = JSONObjectUserType.class) })
public class ValueSetModel {

	@Id
	@Column(name = "id")
	private String valueSetId;

	@Column(name = "data")
	@Type(type = "StringJsonObject")
	private String data;

	@Column(name = "last_updated_ts")
	@Temporal(TemporalType.TIMESTAMP)
	@UpdateTimestamp
	private Date timestamp;

	public String getValueSetId() {
		return valueSetId;
	}

	public void setValueSetId(String valueSetId) {
		this.valueSetId = valueSetId;
	}

	public String getData() {
		return data;
	}

	public void setData(String data) {
		this.data = data;
	}

	public Date getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(Date timestamp) {
		this.timestamp = timestamp;
	}
}
